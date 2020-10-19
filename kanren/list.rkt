#lang racket/base

(require "base.rkt"
         "utility.rkt")

(provide (all-defined-out))

(define (conso f r out)
  (congruent (cons f r) out))

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

(define (membero m l)
  (conde [(firsto m l)]
         [(fresh (r)
                 (resto r l)
                 (membero m r))]))
