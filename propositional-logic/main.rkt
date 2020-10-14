#lang racket/base

(require "parser.rkt"
         "tokenizer.rkt"
         racket/match
         racket/hash
         racket/function
         racket/list
         #;(for-syntax syntax/parse))

(define (simplify ast)
  (match ast
    [(list 'sentence s)          (list 'sentence (simplify s))]
    [(list 'negation s)          (list 'negation (simplify s))]
    [(list 'atom s)              (list 'atom s)]
    [(list symbol s)             (simplify s)]
    [(list symbol expr ..2)      (cons symbol (map simplify expr))]))

(define (logic-eval vars sentence)
  (define eval-with-vars (curry logic-eval vars))
  (match sentence
    [(list 'sentence s) (eval-with-vars s)]
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

(define (read-string s)
  (define stx (parse (tokenize (open-input-string s))))
  (simplify (syntax->datum stx)))

(define vars (make-hash (list (cons "A" #t)
                              (cons "B" #f))))

(read-string "A <-> B v C v D")

(read-string "~P ^ Q v P -> P v Q")

(read-string "(~P v P) ^ QB")

(read-string "Q -> A v B ^ C")

(logic-eval vars (read-string "A ->  B"))
(logic-eval vars (read-string "A -> ~B"))
(logic-eval vars (read-string "A v B"))
(logic-eval vars (read-string "~A v B"))
