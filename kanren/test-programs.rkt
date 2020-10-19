#lang racket/base

(require "kanren.rkt")

(define a-and-b
  (call/empty-state
   (conj
    (call/fresh (lambda (a) (congruent a 7)))
    (call/fresh (lambda (b) (disj
                             (congruent b 5)
                             (congruent b 6)))))))

(define list-test
  (call/empty-state
   (fresh (q p b)
          (congruent p q)
          (congruent b 5)
          (congruent p (list 1 2 3 b)))))

(define append-test
  (call/empty-state
   (fresh (q p)
          (appendo p q '(1 2 3)))))

(define second-append-test
  (take-all
   (call/empty-state
    (fresh (q)
           (fresh (x y)
                  (appendo x y '(1 2 3 4 5))
                  (congruent q (list x y)))))))

(define many-vars
  (call/empty-state
   (call/fresh
    (lambda (q)
      (call/fresh
       (lambda (p)
         (conj
          (congruent q p)
          (congruent p 5))))))))

(define many-vars-fresh
  (call/empty-state
   (fresh (p q)
          (conj
           (congruent p q)
           (congruent p 5)))))

(define impossible-many-vars
  (call/empty-state
   (fresh (q p)
          (conj
           (conj
            (congruent q p)
            (congruent p 5))
           (congruent q 4)))))

(define run-test
  (run* (q x y)
        (appendo x y '(1 2 3 4 5))
        (congruent q (list x y))))

(define run-all-test
  (run/all* (x y)
            (appendo x y '(1 2 3 4 5))))

(define reify-test
  (run* (x y)
        (congruent x y)))

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

(define member-test
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 3 4 5 6))))

(define member-test-2
  (run* (q)
        (membero q (list 1 2 3))
        (membero q (list 4 5 6))))
