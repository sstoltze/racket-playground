#lang racket/base
(require syntax/strip-context
         "tokenizer.rkt"
         "parser.rkt")

(provide (all-defined-out))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module gherkin-module racket
       #,path
       #,parse-tree)))

(define (gherkin->string s)
  (parse-gherkin (open-input-string s)))

(define (parse-gherkin port)
  (parse-to-datum (make-tokenizer port #f)))
