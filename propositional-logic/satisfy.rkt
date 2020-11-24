#lang racket/base

(require "utility.rkt"
         racket/match
         racket/set
         racket/string)

(provide satisfy
         equivalent-propositions?)

(define (satisfy prop)
  (define vars (extract-variables prop))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'kanren)
    (kanren-join-vars vars
                      (eval `(run* (q)
                                   (fresh ,vars
                                          ,(to-kanren prop)
                                          (congruent q ,(cons 'list vars))))))))

(define (equivalent-propositions? p q)
  (set-empty? (set-symmetric-difference (map kanren-equivalent-solution (satisfy p))
                                        (map kanren-equivalent-solution (satisfy q)))))

;; Replace the symbols _.1, _.2, etc. in kanren solutions with 'any, and convert the list of variables to a set,
;; for easy checking of equivalence
(define (kanren-equivalent-solution l)
  (for/set ([v l])
    (match-define (cons var val) v)
    (define updated-val (if (and (symbol? val)
                                 (string-prefix? (symbol->string val) "_."))
                            'any
                            val))
    (cons var updated-val)))

(define (kanren-join-vars vars solutions)
  (for/list ([s (in-list solutions)])
    (for/list ([var (in-list vars)]
               [val (in-list s)])
      (cons var val))))

(define (to-kanren prop)
  (kanren-eval-to prop #t))

(define (kanren-eval-to prop target)
  (match prop
    [(list 'sentence s)         (kanren-eval-to s target)]
    [(list 'biimplication a b)  (if target
                                    `(conde [,(kanren-eval-to a #t)
                                             ,(kanren-eval-to b #t)]
                                            [,(kanren-eval-to a #f)
                                             ,(kanren-eval-to b #f)])
                                    `(conde [,(kanren-eval-to a #t)
                                             ,(kanren-eval-to b #f)]
                                            [,(kanren-eval-to a #f)
                                             ,(kanren-eval-to b #t)]))]
    [(list 'implication a b)    (if target
                                    `(disj+ ,(kanren-eval-to a #f)
                                            ,(kanren-eval-to b #t))
                                    `(conj+ ,(kanren-eval-to a #t)
                                            ,(kanren-eval-to b #f)))]
    [(list 'conjunction as ...) (if target
                                    (cons 'conj+ (map (lambda (a) (kanren-eval-to a #t))
                                                      as))
                                    (cons 'disj+ (map (lambda (a) (kanren-eval-to a #f))
                                                      as)))]
    [(list 'disjunction as ...) (if target
                                    (cons 'disj+ (map (lambda (a) (kanren-eval-to a #t))
                                                      as))
                                    (cons 'conj+ (map (lambda (a) (kanren-eval-to a #f))
                                                      as)))]
    [(list 'negation a)         (kanren-eval-to a (not target))]
    [(list 'atom a)             `(congruent ,(string->symbol a) ,target)]))
