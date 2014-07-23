#lang racket

(require "../racket-utils/tri-partition-set.rkt"
         "../lattice/lattice.rkt"
         racket/contract/region
         racket/generic
         )
(provide filter-out-seen
         initial-seen-set
         )

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
  (define-values (greater-or-eq lesser incomparable)
    (tri-partition-set (lambda (x) (item-gte? x item))
                       (lambda (x) (item-gte? item x))
                       relevant-subset))
  (if (set-empty? greater-or-eq)
      (values #t (seen-set-change-code-set Seen code (set-add incomparable item)))
      (begin (log-info
              "We've already seen:\n  ~a\nwhose related stuff is:\n  ~a\n\n"
              item
              relevant-subset)
             (values #f Seen))))

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
       (gen:gte? ctx ctx2)
       (gen:gte? sigma sigma2)))
