#lang racket

(require "../lattice/lattice.rkt"
         racket/contract/region
         racket/generic
         )
(provide seen-set-add!
         seen-set-member?
         seen-set-average-visits
         seen-set-median-visits
         seen-set-variance-visits
         seen-set-skewness-visits
         seen-set-kurtosis-visits
         initial-seen-set
         )

(require math/statistics)

(define (seen-set-visit-histo s)
  (for/list ([(k v) (in-hash (seen-set-h s))]) (set-count v)))

(define (seen-set-average-visits s)
  (mean (seen-set-visit-histo s)))
(define (seen-set-median-visits s)
  (median < (seen-set-visit-histo s)))
(define (seen-set-variance-visits s)
  (variance (seen-set-visit-histo s)))
(define (seen-set-skewness-visits s)
  (skewness (seen-set-visit-histo s)))
(define (seen-set-kurtosis-visits s)
  (kurtosis (seen-set-visit-histo s)))

(struct seen-set (h)
        #:transparent
        #:methods gen:set
        [(define/generic generic-set-count set-count)
         (define/generic generic-set->stream set->stream)
         (define/generic generic-set-union set-union)
         (define/generic generic-set-first set-first)
         (define/generic generic-set-remove! set-remove!)
         (define/generic generic-set-empty? set-empty?)
         (define (set-count s)
           (for/sum ([(ctx st) (in-hash (seen-set-h s))])
             (generic-set-count st)))
         (define (set->stream s)
           (generic-set->stream
            (for/fold ([seen (set)]) ([(code states) (in-hash (seen-set-h s))])
              (generic-set-union seen states))))])

(define (empty-seen-set) (seen-set (make-hash)))
(define (make-empty-relevant-set) (mutable-set))

(define (initial-seen-set initial-work-set)
  (define ss (empty-seen-set))
  (for ([item initial-work-set])
    (seen-set-add! ss item))
  ss)
(define (seen-set-relevant-subset s ctx code)
  (define h (seen-set-h s))
  (hash-ref h (cons ctx code)
            (lambda ()
              (define new (make-empty-relevant-set))
              (hash-set! h (cons ctx code) new)
              new)))
(define (seen-set-add! Seen item)
  (match-define (list ctx sigma code) item)
  (define relevant-subset (seen-set-relevant-subset Seen ctx code))
  (set-add! relevant-subset item))

(define (seen-set-member? Seen item)
  (match-define (list ctx sigma code) item)
  (define relevant-subset (seen-set-relevant-subset Seen ctx code))
  (set-member? relevant-subset item))
