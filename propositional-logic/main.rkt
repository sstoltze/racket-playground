#lang racket/base

(require "reader.rkt"
         "utility.rkt"
         "satisfy.rkt")

(define vars (make-hash (list (cons "A" #t)
                              (cons "B" #f))))

(string->proposition "A <-> B v C v D")

(string->proposition "~P ^ Q v P -> P v Q")

(string->proposition "(~P v P) ^ QB")

(string->proposition "Q -> A v B ^ C")

(extract-vars (string->proposition "Q -> A v B ^ C"))
(satisfy (string->proposition "Q -> A v B ^ C"))

(string->proposition "Q -> A")
(satisfy (string->proposition "A v B ^ C"))

(logic-eval vars (string->proposition "A ->  B"))
(logic-eval vars (string->proposition "A -> ~B"))
(logic-eval vars (string->proposition "A v B"))
(logic-eval vars (string->proposition "~A v B"))
