#lang racket/base

(require "tokenizer.rkt"
         "parser.rkt"
         racket/match)

(provide (except-out (all-defined-out)
                     keep-symbol?))

(define (keep-symbol? s)
  (match s
    [(or 'sentence 'negation 'atom) #t]
    [_                              #f]))

(define (simplify ast)
  (match ast
    ;; Preserve sentences, negations and atoms, since these are important
    [(list (? keep-symbol? logic-expr)  statement) (list logic-expr (simplify statement))]
    ;; Conjunctions, disjunctions, etc. can be thrown away if they only have a single part
    [(list logic-expr statement)                   (simplify statement)]
    ;; Otherwise, keep them but simplify the subexpressions
    [(list logic-expr statements ..2)              (cons logic-expr (map simplify statements))]
    ;; Keep unknowns
    [_                                             ast]))

(define (read-string s)
  (define stx (parse (tokenize (open-input-string s))))
  (simplify (syntax->datum stx)))
