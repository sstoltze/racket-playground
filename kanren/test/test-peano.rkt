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
