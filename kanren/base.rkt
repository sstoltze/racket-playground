#lang racket/base

(require "types.rkt")
(provide (all-defined-out))

(define (walk u s)
  (define pair (and (var? u)
                    (lookup-state s u)))
  (if (binding? pair)
      (walk (binding-value pair) s)
      u))

(define (ext-s x v s)
  (extend-state s x v))

(define (unify u v s)
  (define walked-u (walk u s))
  (define walked-v (walk v s))
  (cond
    [(and (var?  walked-u)
          (var?  walked-v)
          (var=? walked-u walked-v))
     s]
    [(var? walked-u)
     (ext-s u v s)]
    [(var? walked-v)
     (ext-s v u s)]
    ;; Unify a list by unifying cars and then unifying cdrs
    [(and (pair? walked-u)
          (pair? walked-v))
     (define new-s (unify (car walked-u) (car walked-v) s))
     (and new-s
          (unify (cdr walked-u) (cdr walked-v) new-s))]
    [else
     (and (equal? walked-u walked-v)
          s)]))

(define (congruent u v)
  (lambda (s)
    (define new-s (unify u v s))
    (if new-s
        (unit new-s)
        mzero)))

(define == congruent)

(define (call/fresh f)
  (lambda (s)
    (define-values (c new-s) (fresh-var s))
    ((f c) new-s)))

(define (call/empty-state goal)
  (goal empty-state))

(define (disj f g)
  (lambda (s)
    (mplus (f s) (g s))))

(define (conj f g)
  (lambda (s)
    (bind (f s) g)))

(define (mplus stream1 stream2)
  (cond
    [(stream-empty? stream1)    stream2]
    [(stream-immature? stream1) (lambda () (mplus stream2 (stream-realize stream1)))]
    [else                       (stream-add (stream-first stream1) (mplus stream2 (stream-rest stream1) ))]))

(define (bind stream goal)
  (cond
    [(stream-empty? stream)    mzero]
    [(stream-immature? stream) (lambda () (bind (stream-realize stream) goal))]
    [else                      (mplus (goal (stream-first stream)) (bind (stream-rest stream) goal))]))
