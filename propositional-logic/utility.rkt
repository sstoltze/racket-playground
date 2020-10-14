#lang racket/base

(require racket/function
         racket/match
         racket/list)

;; TODO: Reduce this
(provide (all-defined-out))

(define (logic-eval vars sentence)
  (define eval-with-vars (curry logic-eval vars))
  (match sentence
    [(list 'sentence s)         (eval-with-vars s)]
    [(list 'biimplication a b)  (biimplication (eval-with-vars a) (eval-with-vars b))]
    [(list 'implication a b)    (implication (eval-with-vars a) (eval-with-vars b))]
    [(list 'disjunction as ...) (list-or (map eval-with-vars as))]
    [(list 'conjunction as ...) (list-and (map eval-with-vars as))]
    [(list 'negation a)         (not (eval-with-vars a))]
    [(list 'atom a)             (hash-ref vars a)]))

(define (apply-op op default)
  (letrec ([recursive-op (lambda (lst)
                           (if (empty? lst)
                               default
                               (op (first lst) (recursive-op (rest lst)))))])
    recursive-op))

(define list-or
  (apply-op (lambda (a b) (or a b)) #f))

(define list-and
  (apply-op (lambda (a b) (and a b)) #t))

(define (biimplication a b)
  (or (and a b)
      (and (not a) (not b))))

(define (implication a b)
  (or b (not a)))
