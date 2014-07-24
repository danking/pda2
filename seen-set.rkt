#lang racket

(require "../lattice/lattice.rkt"
         racket/contract/region
         racket/generic
         )
(provide filter-out-seen
         initial-seen-set
         ctx-gte?
         sigma-gte?
         )

(define ctx-gte?
  (make-parameter
   (lambda (x y) (error 'ctx-gte "please parameterize by ctx-gte?"))))
(define sigma-gte?
  (make-parameter
   (lambda (x y) (error 'sigma-gte "please parameterize by sigma-gte?"))))

(struct seen-set (h)
        #:transparent
        #:methods gen:set
        [(define/generic generic-set-count set-count)
         (define/generic generic-set->stream set->stream)
         (define/generic generic-set-union set-union)
         (define (set-count s)
           (for/sum ([(ctx st) (seen-set-h s)])
             (generic-set-count st)))
         (define (set->stream s)
           (generic-set->stream
            (for/fold ([seen (set)]) ([(code states) (seen-set-h s)])
              (generic-set-union seen states))))])

(define (initial-seen-set initial-work-set)
  (for/fold
      ([ss (seen-set (hash))])
      ([item initial-work-set])
    (let-values (((_ ss*) (seen-set-add ss item))) ss*)))
(define (seen-set-relevant-subset s c)
  (hash-ref (seen-set-h s) c (set)))
(define (seen-set-change-code-set s c sts)
  (seen-set (hash-set (seen-set-h s) c sts)))
(define (seen-set-add Seen item)
  (match-define (list ctx sigma code) item)
  (define relevant-subset (seen-set-relevant-subset Seen code))
  (define already-known (for/or ([x relevant-subset]) (item-gte? x item)))
  (cond [already-known
         (log-info "We already know:\n  ~a\nwhose related stuff is:\n  ~a\n\n"
                   item
                   relevant-subset)
         (values #f Seen)]
        [else
         (define incomparables (set-filter (lambda (x) (not (item-gte? item x)))
                                           relevant-subset))
         (values #t (seen-set-change-code-set Seen
                                              code
                                              (set-add incomparables item)))]))

(define (filter-out-seen news Seen)
  (for/fold
      ([fresh-news (set)]
       [Seen* Seen])
      ([item news])
    (let-values (((changed? Seen**) (seen-set-add Seen* item)))
      (values (if changed?
                  (set-add fresh-news item)
                  fresh-news)
              Seen**))))

(define (item-gte? l r)
  (match-define (list ctx sigma code) l)
  (match-define (list ctx2 sigma2 code2) r)

  (and (equal? code code2)
       ((ctx-gte?) ctx ctx2)
       ((sigma-gte?) sigma sigma2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Utilities

(define (set-filter p s)
  (for/fold
      ([new (set)])
      ([e (in-set s)])
    (if (p e) (set-add new e) new)))
