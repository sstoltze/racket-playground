#lang racket/base

(require "../kanren/main.rkt"
         "utility.rkt"
         racket/match)

(provide satisfy)

(define (satisfy prop)
  (define vars (extract-variables prop))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require "../kanren/main.rkt")
    (kanren-join-vars vars
                      (eval `(run* (q)
                                   (fresh ,vars
                                          ,(to-kanren prop)
                                          (congruent q ,(cons 'list vars))))))))

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
