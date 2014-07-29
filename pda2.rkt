#lang racket

(require "flow-analysis-struct.rkt"
         "work-set.rkt"
         "../lattice/lattice.rkt")
(provide PDA2 FlowAnalysis)

(require "../pda-to-pda-risc/risc-enhanced/data.rkt"
         math/statistics
         "../semantics/abstract.rkt"
         (only-in "../semantics/context-data.rkt"
                  context-push
                  context-top-of-stack))


;; W : Context x State x Code
;; Seen : Context x State x Code

(define (PDA2 FA)
  (match-define (FlowAnalysis flow
                              flow-ctx
                              flow-across
                              user-ctx-gte?
                              user-sigma-gte?
                              initial-states
                              initial-configuration
                              initial-ctx-state)
                FA)

  (parameterize ([ctx-gte? user-ctx-gte?]
                 [sigma-gte? user-sigma-gte?])

    (define W (initial-work-set initial-states))
    (define Seen (mutable-set))
    (for ([item (in-set initial-states)]) (set-add! Seen item))

    (define i 0)
    (define samples 0)
    (define start-time (current-milliseconds))
    (define previous-time start-time)
    (define previous-size (set-count Seen))
    (define running-average-speed 0)
    (define PRINT_FREQUENCY 5000)

    (printf "(W\t Seen\t ContextState\t t(ms)\t Δt(ms)\t ΔSize\t Rate(1/ms) AvgRate Samples i)\n")

    (define (loop Configuration CtxState)
      (cond
       [(set-empty? W) (values Seen Configuration CtxState)]
       [else (match-define (list ctx sigma code) (set-first! W))
             (set! i (add1 i))
             (when (= (modulo i PRINT_FREQUENCY) 0)
               (set! samples (add1 samples))
               (define time-now (current-milliseconds))
               (define size-now (set-count Seen))
               (define time-diff (- time-now previous-time))
               (define size-diff (- size-now previous-size))
               (set! previous-time time-now)
               (set! previous-size (set-count Seen))
               (define speed-now (exact->inexact (/ size-diff time-diff)))
               (set! running-average-speed
                     (+ (/ (* (sub1 samples) running-average-speed) samples)
                        (/ speed-now samples)))
               (printf "(~a\t ~a\t ~a\t ~a\t ~a\t ~a\t ~a\t ~a\t ~a)   ~a\n"
                       (set-count W)
                       (set-count Seen)
                       (ctx-state-callers&summaries-count CtxState)
                       (- time-now start-time)
                       time-diff
                       size-diff
                       (str&trunc 6 speed-now)
                       (str&trunc 6 running-average-speed)
                       samples
                       i
                       sigma))
             (when (= (modulo i (* 20 PRINT_FREQUENCY)) 0)
               (printf "::\n")
               (define x (make-hash))
               (for ((item (in-set Seen)))
                 (match-define (list ctx sigma code) item)
                 (hash-set! x
                            (pda-term->uid code)
                            (add1 (hash-ref x (pda-term->uid code) 0))))
               (define ls (for/list (((k v) x)) v))
               (printf "Mean Median Variance: ~a ~a ~a\n"
                       (exact->inexact (mean ls))
                       (exact->inexact (median < ls))
                       (exact->inexact (variance ls)))
               (printf "Top 20: ~a\n"
                       (take (sort ls >) 20))
               (define ls2 (sort (for/list (((k v) x)) (list k v))
                                 (lambda (x y)
                                   (> (second x) (second y)))))
               (define contexts (make-hash))
               (for ((item (in-set Seen)))
                 (match-define (list ctx sigma code) item)
                 (hash-set! contexts
                            (pda-term->uid code)
                            (set-add (hash-ref contexts
                                               (pda-term->uid code)
                                               (set))
                                     ctx)))
               (printf "Contexts Top 20: ~a\n"
                       (map (lambda (p)
                              (set-count (hash-ref contexts (first p))))
                            (take ls2 20)))
               (define regenvs (make-hash))
               (for ((item (in-set Seen)))
                 (match-define (list ctx sigma code) item)
                 (hash-set! regenvs
                            (pda-term->uid code)
                            (set-add (hash-ref regenvs
                                               (pda-term->uid code)
                                               (set))
                                     (abstract-state-re sigma))))
               (printf "RegEnvs Top 20: ~a\n"
                       (map (lambda (p)
                              (set-count (hash-ref regenvs (first p))))
                            (take ls2 20)))
               (define pushes (make-hash))
               (for ((item (in-set Seen)))
                 (match-define (list ctx sigma code) item)
                 (hash-set! pushes
                            (pda-term->uid code)
                            (set-add (hash-ref pushes
                                               (pda-term->uid code)
                                               (set))
                                     (context-push ctx))))
               (printf "Pushes Top 20: ~a\n"
                       (map (lambda (p)
                              (set-count (hash-ref pushes (first p))))
                            (take ls2 20)))
               (for ((c (map first (take ls2 20))))
                 (printf "UID: ~a\t" c)
                 (printf "  ~a\n" (set-map (hash-ref pushes c)
                                           (lambda (x) (or (not x)
                                                           (pda-term->uid x))))))
               (printf "::\n"))

             (define-values (new-ctx CtxState* Configuration*)
               ((flow-ctx code) ctx sigma CtxState Configuration))
             (define-values (news Configuration**)
               ((flow code) ctx new-ctx sigma Configuration*))
             (define-values (news-across CtxState** Configuration***)
               (flow-across ctx new-ctx CtxState* Configuration**))

             (for* ([item (sequence-append (in-set news) (in-set news-across))])
               (unless (set-member? Seen item)
                 (set-add! Seen item)
                 (work-set-add! W item)))

             (loop Configuration*** CtxState**)]))

    (loop initial-configuration initial-ctx-state)))


(define (set-first! s)
  (let ((e (set-first s)))
    (set-remove! s e)
    e))

(define (str&trunc n v)
  (let ((str (format "~a" v)))
    (substring str 0 (min n (string-length str)))))
