#lang racket

(require "flow-analysis-struct.rkt"
         "seen-set.rkt"
         "../lattice/lattice.rkt")
(provide PDA2 FlowAnalysis)


;; W : Context x State x Code
;; Seen : Context x State x Code

(define (PDA2 FA)
  (match-define (FlowAnalysis flow
                              flow-ctx
                              flow-across
                              initial-work-set
                              initial-configuration
                              initial-ctx-state)
                FA)

  (define (loop W Seen Configuration CtxState)
    (cond
     [(set-empty? W) (values Seen Configuration CtxState)]
     [else (match-define (list ctx sigma code) (set-first W))
           (define W* (set-rest W))

           (define-values (ctx*s CtxState* Configuration*)
             ((flow-ctx code) ctx sigma CtxState Configuration))
           (define-values (news Configuration**)
             ((flow code) ctx sigma Configuration*))
           (define-values (news-across CtxState** Configuration***)
             (flow-across ctx ctx*s CtxState* Configuration**))
           (define contextualized-news
             (for*/set ([item (set-union news news-across)]
                        [ctx* ctx*s])
               (cons ctx* item)))

           (define-values (fresh-news Seen*)
             (filter-out-seen contextualized-news Seen))

           (define W** (set-union fresh-news W*))

           (loop W** Seen* Configuration*** CtxState**)]))

  (loop initial-work-set (initial-seen-set initial-work-set)
        initial-configuration initial-ctx-state))
