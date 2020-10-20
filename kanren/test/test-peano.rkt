#lang racket/base

(require "../kanren.rkt")

(define plus-test
  (run* (n)
        (pluso 2 2 n)))

(define plus-test-2
  (run* (q)
        (fresh (n m)
               (pluso n m 5)
               (congruent q (list n m)))))

(define succ-test
  (run* (q)
        (fresh (n)
               (congruent n 5)
               (succo q n))))

(define succ-test-2
  (run* (q)
        (succo 5 4)))

(define succ-test-3
  (run* (q)
        (succo 5 3)))

(define anoter-plus-test
  (run 5 (q)
       (fresh (n m)
              (pluso n 3 m)
              (congruent q (list n m)))))

(define subtract-test
  (run 5 (q)
       (fresh (n m)
              (subtracto n 3 m)
              (congruent q (list n m)))))

;; Multiplication takes forever.
(define mult-test
  (map from-peano (run 2 (q)
                       (multo 4 3 q))))

(define mult-test-2
  (map from-peano (run/all* (q)
                        (multo q 3 3))))

(define mult-test-3
  (map from-peano (run 4 (q)
                       (multo q 3 0))))

(define mult-test-4
  (run/all 4 (q p)
           (multo q p 6)))
