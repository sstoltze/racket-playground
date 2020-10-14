#lang racket/base

(require brag/support
         br-parser-tools/lex)
(provide tokenize)

(define (tokenize input-port)
  (define lexer
    (lexer-src-pos
     [(repetition 1 +inf.0 upper-case) (token 'ATOMIC lexeme)] ; Atoms are upper case strings
     ["^" (token 'AND lexeme)]
     ["v" (token 'OR lexeme)]
     [(union "!" "~") (token 'NOT lexeme)]
     ["->" (token 'IMPLIES lexeme)]
     ["<->" (token 'IFF lexeme)]
     ["(" (token 'LPAR lexeme)]
     [")" (token 'RPAR lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (lexer input-port))
  next-token)
