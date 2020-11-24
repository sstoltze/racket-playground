#lang racket/base

(require "base.rkt"
         "utility.rkt"
         "peano.rkt")

(provide (all-defined-out))

(define (conso f r out)
  (congruent out (cons f r)))

(define (listo l)
  (conde [(emptyo l)]
         [(nonemptyo l)]))

(define (nonemptyo l)
  (fresh (f r)
         (conso f r l)))

(define (firsto f out)
  (fresh (r)
         (conso f r out)))

(define (resto r out)
  (fresh (f)
         (conso f r out)))

(define (emptyo x)
  (congruent x '()))

(define (appendo a b out)
  (conde [(emptyo a) (congruent b out)]
         [(fresh (f r rec)
                 (conso f r a)
                 (conso f rec out)
                 (appendo r b rec))]))

(define (lengtho n out)
  (conde [(zeroo n) (emptyo out)]
         [(fresh (f r m)
                 (conso f r out)
                 (succo n m)
                 (lengtho m r))]))

(define (membero m l)
  (conde [(firsto m l)]
         [(fresh (r)
                 (resto r l)
                 (membero m r))]))
