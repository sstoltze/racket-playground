#lang racket/base

(require "types.rkt"
         "base.rkt"
         "utility.rkt"
         (for-syntax syntax/parse
                     racket/base))

(provide (all-defined-out))

(define zero 0)

(define (succ n)
  (cond [(and (number? n)
              (> n 0)) (succ (to-peano n))]
        [else          (list 'S n)]))

(define (numbero n)
  (or (congruent n zero)
      (fresh (q)
             (congruent n (succ q)))))

(define (to-peano k)
  (if (number? k)
      (if (equal? k zero)
          zero
          (succ (to-peano (sub1 k))))
      k))

(define (from-peano n)
  (if (zero? n)
      0
      (add1 (from-peano (car n)))))

(define-syntax (define/peano stx)
  (syntax-parse stx
    [(_ (f x ...) expr ...)
     #'(define (f x ...)
         (let [(x (to-peano x)) ...]
           expr ...))]))

(define/peano (succo n m)
  (congruent n (succ m)))

(define (zeroo n)
  (congruent n zero))

(define/peano (pluso n m out)
  (conde [(zeroo n) (congruent m out)]
         [(fresh (x z)
                 (succo n x)
                 (succo out z)
                 (pluso x m z))]))
