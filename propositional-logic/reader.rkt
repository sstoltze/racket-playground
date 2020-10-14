#lang racket/base

(require "tokenizer.rkt"
         "parser.rkt"
         racket/match)

(provide (all-defined-out))

(define (simplify ast)
  (match ast
    [(list 'sentence s)          (list 'sentence (simplify s))]
    [(list 'negation s)          (list 'negation (simplify s))]
    [(list 'atom s)              (list 'atom s)]
    [(list symbol s)             (simplify s)]
    [(list symbol expr ..2)      (cons symbol (map simplify expr))]))

(define (read-string s)
  (define stx (parse (tokenize (open-input-string s))))
  (simplify (syntax->datum stx)))
