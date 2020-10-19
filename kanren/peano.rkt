#lang racket/base

(require "types.rkt"
         "base.rkt"
         "utility.rkt")

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
  (if (equal? k zero)
      zero
      (succ (to-peano (sub1 k)))))

(define (from-peano n)
  (if (zero? n)
      0
      (add1 (from-peano (car n)))))

(define (succo n m)
  (cond
    [(number? n) (succo (to-peano n) m)]
    [(number? m) (succo n (to-peano m))]
    [else (congruent n (succ m))]))

(define (zeroo n)
  (congruent n zero))

(define (pluso n m out)
  (cond
    [(number? n)   (pluso (to-peano n) m out)]
    [(number? m)   (pluso n (to-peano m) out)]
    [(number? out) (pluso n m (to-peano out))]
    [else          (conde [(zeroo n) (congruent m out)]
                          [(fresh (x z)
                                  (succo n x)
                                  (succo out z)
                                  (pluso x m z))])]))
