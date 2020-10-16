#lang racket/base

(require "kanren.rkt")

(define a-and-b
  (call/empty-state
   (conj
    (call/fresh (lambda (a) (congruent a 7)))
    (call/fresh (lambda (b) (disj
                             (congruent b 5)
                             (congruent b 6)))))))


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
