#lang racket/base

(require "reader.rkt"
         "utility.rkt")

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
