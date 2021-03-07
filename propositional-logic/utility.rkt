#lang racket/base

(provide extract-variables)

(require racket/match
         racket/list)

(define (extract-variables prop)
  (remove-duplicates
   (match prop
     [(list 'sentence s) (extract-variables s)]
     [(list 'atom a)     (list (string->symbol a))]
     [(list _ as ...)    (append-map extract-variables as)])))
