#lang racket/base

(require "../kanren.rkt")

(define member-test
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 3 4 5 6))))

(define member-test-2
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 4 5 6))))

(define append-test
  (run* (q p)
        (appendo p q '(1 2 3))))

(define second-append-test
  (run* (q)
        (fresh (x y)
               (appendo x y '(1 2 3 4 5))
               (congruent q (list x y)))))

(define list-test
  (run* (q p b)
        (congruent q p)
        (congruent b 5)
        (congruent p (list 1 2 3 b))))
