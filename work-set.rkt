#lang racket

(require "../lattice/lattice.rkt"
         "../pda-to-pda-risc/risc-enhanced/fast-equal.rkt"
         racket/contract/region
         racket/generic
         )
(provide work-set-add!
         work-set-average-visits
         work-set-median-visits
         work-set-variance-visits
         work-set-skewness-visits
         work-set-kurtosis-visits
         initial-work-set
         ctx-gte?
         sigma-gte?
         )

(require math/statistics)

(define ctx-gte?
  (make-parameter
   (lambda (x y) (error 'ctx-gte "please parameterize by ctx-gte?"))))
(define sigma-gte?
  (make-parameter
   (lambda (x y) (error 'sigma-gte "please parameterize by sigma-gte?"))))

(define (work-set-visit-histo s)
  (for/list ([(k v) (in-hash (work-set-h s))]) (set-count v)))

(define (work-set-average-visits s)
  (mean (work-set-visit-histo s)))
(define (work-set-median-visits s)
  (median < (work-set-visit-histo s)))
(define (work-set-variance-visits s)
  (variance (work-set-visit-histo s)))
(define (work-set-skewness-visits s)
  (skewness (work-set-visit-histo s)))
(define (work-set-kurtosis-visits s)
  (kurtosis (work-set-visit-histo s)))

(struct work-set (h)
        #:methods gen:set
        [(define/generic generic-set-count set-count)
         (define/generic generic-set->stream set->stream)
         (define/generic generic-set-union set-union)
         (define/generic generic-set-first set-first)
         (define/generic generic-set-remove! set-remove!)
         (define/generic generic-set-empty? set-empty?)
         (define (set-count s)
           (for/sum ([(ctx st) (in-hash (work-set-h s))])
             (generic-set-count st)))
         (define (set-first s)
           (define h (work-set-h s))
           (generic-set-first (hash-iterate-value h (hash-iterate-first h))))
         (define (set-remove! s e)
           (let ((relevant-subset (work-set-relevant-subset s (third e))))
             (cond [(= 1 (generic-set-count relevant-subset))
                    (work-set-remove-relevant-subset! s (third e))]
                   [else (generic-set-remove! relevant-subset e)])))
         (define (set-empty? s)
           (hash-empty? (work-set-h s)))
         (define (set->stream s)
           (generic-set->stream
            (for/fold ([seen (set)]) ([(code states) (in-hash (work-set-h s))])
              (generic-set-union seen states))))])

(define (empty-work-set) (work-set (make-hash)))
(define (make-empty-relevant-set) (mutable-set))

(define (initial-work-set states)
  (define ss (empty-work-set))
  (for ([state (in-set states)])
    (work-set-add! ss state))
  ss)
(define (work-set-remove-relevant-subset! s c)
  (hash-remove! (work-set-h s) c))
(define (work-set-relevant-subset s c)
  (hash-ref (work-set-h s) c
            (lambda ()
              (log-debug "Didn't find a set for ~a in ~a\n" c s)
              (define new (make-empty-relevant-set))
              (hash-set! (work-set-h s) c new)
              (log-debug "Now you should see it mapped in ~a\n\n" s)
              new)))
(define (work-set-add! Seen item)
  (match-define (list ctx sigma code) item)
  (define relevant-subset (work-set-relevant-subset Seen code))
  (define already-known (for/or ([x (in-set relevant-subset)]) (item-gte? x item)))
  (cond [already-known
         (log-debug "We already know:\n  ~a\nwhose related stuff is:\n  ~a\n\n"
                   item
                   relevant-subset)
         #f]
        [else
         (log-debug "Before filtering: ~a\n" relevant-subset)
         (set-filter! relevant-subset (lambda (x) (not (item-gte? item x))))
         (log-debug "After filtering: ~a\n" relevant-subset)
         (set-add! relevant-subset item)
         (log-debug "After add: ~a\n\n" relevant-subset)
         #t]))

(define (item-gte? l r)
  (match-define (list ctx sigma code) l)
  (match-define (list ctx2 sigma2 code2) r)

  (and (fast-term-equal? code code2)
       ((ctx-gte?) ctx ctx2)
       ((sigma-gte?) sigma sigma2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Utilities

(define (set-filter! s p)
  (for ([e (in-list (set->list s))])
    (unless (p e)
      (set-remove! s e))))
