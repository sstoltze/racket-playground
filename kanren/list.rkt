#lang racket/base

(require "base.rkt"
         "utility.rkt"
         "peano.rkt")

(provide (all-defined-out))

(define (conso f r out)
  (congruent out (cons f r)))

(define (emptyo x)
  (congruent x '()))

(define (nonemptyo l)
  (fresh (f r)
         (conso f r l)))

(define (listo l)
  (conde [(emptyo l)]
         [(nonemptyo l)]))

(define nonlisto (make-kanren-predicate (compose not list?)))

(define (firsto f out)
  (fresh (r)
         (conso f r out)))

(define (resto r out)
  (fresh (f)
         (conso f r out)))

(define (appendo a b out)
  (conde [(emptyo a) (congruent b out)]
         [(fresh (f r rec)
                 (conso f r a)
                 (conso f rec out)
                 (appendo r b rec))]))

(define (lengtho n l)
  (conde [(zeroo n) (emptyo l)]
         [(fresh (f r m)
                 (conso f r l)
                 (succo n m)
                 (lengtho m r))]))

(define (membero m l)
  (conde [(firsto m l)]
         [(fresh (r)
                 (resto r l)
                 (membero m r))]))

(define (flatteno l o)
  (conde [(emptyo l) (emptyo o)]
         [(fresh (f r r-flat)
                 (conso f r l)
                 (flatteno r r-flat)
                 (conde [(listo f)
                         (fresh (f-flat)
                                (flatteno f f-flat)
                                (appendo f-flat r-flat o))]
                        [(nonlisto f)
                         (conso f r-flat o)]))]))
