#lang racket/base

(require "../main.rkt"
         racket/function)

(define vars (make-hash (list (cons "A" #t)
                              (cons "B" #f))))

(string->proposition "A <-> B v C v D")

(string->proposition "~P ^ Q v P -> P v Q")

(string->proposition "(~P v P) ^ QB")

(string->proposition "Q -> A v B ^ C")

(extract-variables (string->proposition "Q -> A v B ^ C"))
(satisfy (string->proposition "Q -> A v B ^ C"))

(string->proposition "Q -> A")
(satisfy (string->proposition "A v B ^ C"))

(eval-proposition vars (string->proposition "A ->  B"))
(eval-proposition vars (string->proposition "A -> ~B"))
(eval-proposition vars (string->proposition "A v B"))
(eval-proposition vars (string->proposition "~A v B"))

(define prop (string->proposition "Q -> A v B ^ C"))

(define solutions (satisfy prop))

(define (solution-vars->strings vars)
  (for/hash ([v (in-list vars)])
    (values (symbol->string (car v)) (cdr v))))

(for/list ([vars (in-list solutions)])
  (eval-proposition (solution-vars->strings vars) prop))
