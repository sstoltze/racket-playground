#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "types.rkt"
         "base.rkt"
         "stream.rkt"
         "reify.rkt")

(provide (all-defined-out))

(define-syntax (fresh stx)
  (syntax-parse stx
    [(_ () g ...)         #'(conj+ g ...)]
    [(_ (x xs ...) g ...) #'(call/fresh (lambda (x) (fresh (xs ...) g ...)))]))

(define-syntax (inverse-eta-delay stx)
  (syntax-parse stx
    [(_ g) #'(lambda (state) (lambda () (g state)))]))

(define-syntax (conj+ stx)
  (syntax-parse stx
    [(_ g)         #'(inverse-eta-delay g)]
    [(_ g1 g2 ...) #'(conj (inverse-eta-delay g1) (conj+ g2 ...))]))

(define-syntax (disj+ stx)
  (syntax-parse stx
    [(_ g)         #'(inverse-eta-delay g)]
    [(_ g1 g2 ...) #'(disj (inverse-eta-delay g1) (disj+ g2 ...))]))

(define-syntax (conde stx)
  (syntax-parse stx
    [(_ (g ...) ...) #'(disj+ (conj+ g ...) ...)]))

;; Support using _ for wildcards in matche expressions
(define-syntax (match-congruent stx)
  (syntax-parse stx
    #:datum-literals (_)
    [(match-congruent x _) #'unit]
    [(match-congruent x y) #'(congruent x y)]))

(define-syntax (matche stx)
  (syntax-parse stx
    [(_ term (q g ...) ...) #'(conde [(match-congruent term q) g ...] ...)]))

(define-syntax (run stx)
  (syntax-parse stx
    [(_ n (x ...) g0 g ...) #'(miniKanren-reify
                               (take n (call/empty-state
                                        (fresh (x ...)
                                               g0 g ...))))]))

(define-syntax (run* stx)
  (syntax-parse stx
    [(_ (x ...) g0 g ...) #'(miniKanren-reify
                             (take-all (call/empty-state
                                        (fresh (x ...)
                                               g0 g ...))))]))

(define-syntax (run/all stx)
  (syntax-parse stx
    [(_ n (x ...) g0 g ...) #'(reify-all
                               (take n (call/empty-state
                                        (fresh (x ...)
                                               g0 g ...))))]))

(define-syntax (run/all* stx)
  (syntax-parse stx
    [(_ (x ...) g0 g ...) #'(reify-all
                             (take-all (call/empty-state
                                        (fresh (x ...)
                                               g0 g ...))))]))

(define (make-kanren-predicate p)
  (lambda args
    (lambda (s)
      (define new-args (map (lambda (u)
                              (walk u s))
                            args))
      (if (apply p new-args)
          (unit s)
          mzero))))
