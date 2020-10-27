#lang racket/base

(provide eval-proposition)

(require racket/function
         racket/match
         racket/list)

(define (eval-proposition vars sentence)
  (define eval-with-vars (curry eval-proposition vars))
  (match sentence
    [(list 'sentence s)         (eval-with-vars s)]
    [(list 'biimplication a b)  (eval-biimplication (eval-with-vars a) (eval-with-vars b))]
    [(list 'implication a b)    (eval-implication (eval-with-vars a) (eval-with-vars b))]
    [(list 'disjunction as ...) (eval-or (map eval-with-vars as))]
    [(list 'conjunction as ...) (eval-and (map eval-with-vars as))]
    [(list 'negation a)         (not (eval-with-vars a))]
    [(list 'atom a)             (hash-ref vars a)]))

(define (apply-op op default)
  (letrec ([recursive-op (lambda (lst)
                           (if (empty? lst)
                               default
                               (op (first lst) (recursive-op (rest lst)))))])
    recursive-op))

(define eval-or
  (apply-op (lambda (a b) (or a b)) #f))

(define eval-and
  (apply-op (lambda (a b) (and a b)) #t))

(define (eval-biimplication a b)
  (or (and a b)
      (and (not a) (not b))))

(define (eval-implication a b)
  (or b (not a)))
