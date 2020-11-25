#lang racket/base

(require "../main.rkt")

(define member-test
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 3 4 5 6))))

(define member-test-2
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 4 5 6))))

(define member-test-3
  (run* (q)
        (membero q (list 1 2 3 4 5))
        (membero q (list 4 5 6))))

(define append-test
  (run* (q)
        (appendo q '(3) '(1 2 3))))

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

(define listo-test
  (run* (q)
        (listo q)))

(define empty-test
  (run* (q)
        (emptyo q)))

(define nonempty-test
  (run* (q)
        (nonemptyo q)))

;; Find all lists of length 3 containing 1 2 3
(define length-test
  (run* (q)
        (lengtho 3 q)
        (membero 1 q)
        (membero 2 q)
        (membero 3 q)))

(define nonlist-test
  (run* (q)
        (== q '(1 2))
        (nonlisto q)))

(define flatten-test
  (run* (q)
        (flatteno '(1 (2 3) (4 (5 6)) 7) q)))

(define flatten-test-2
  (run 10 (q)
       (flatteno q (list 1 2))))
