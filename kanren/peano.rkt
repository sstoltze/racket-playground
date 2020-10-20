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

(define (oneo n)
  (congruent n (succ 0)))

(define/peano (pluso n m out)
  (conde [(zeroo n) (congruent m out)]
         [(fresh (x z)
                 (succo n x)
                 (succo out z)
                 (pluso x m z))]))

(define/peano (subtracto n m out)
  (pluso m out n))

(define/peano (multo n m out)
  (conde [(zeroo n) (zeroo out)]
         [(zeroo m) (zeroo out)]
         [(oneo n) (congruent m out)]
         [(oneo m) (congruent n out)]
         [(fresh (x z)
                 ;; n = x + 1, z + m = out
                 ;; =>
                 ;; x * m + m = n * m = out = z + m
                 ;; <=>
                 ;; x * m = z
                 (succo n x)
                 (pluso z m out)
                 (multo x m z))]))
