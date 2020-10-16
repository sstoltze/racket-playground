#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "types.rkt"
         "base.rkt")

(provide (all-defined-out))

(define-syntax (fresh stx)
  (syntax-parse stx
    [(_ () g ...) #'(conj+ g ...)]
    [(_ (x xs ...) g ...) #'(call/fresh (lambda (x) (fresh (xs ...) g ...)))]))

(define-syntax (inverse-eta-delay stx)
  (syntax-parse stx
    [(_ g) #'(lambda (state) (lambda () (g state)))]))

(define-syntax (conj+ stx)
  (syntax-parse stx
    [(_ g) #'(inverse-eta-delay g)]
    [(_ g1 g2 ...) #'(conj (inverse-eta-delay g1) (conj+ g2 ...))]))

(define-syntax (disj+ stx)
  (syntax-parse stx
    [(_ g) #'(inverse-eta-delay g)]
    [(_ g1 g2 ...) #'(disj (inverse-eta-delay g1) (disj+ g2 ...))]))

(define-syntax (conde stx)
  (syntax-parse stx
    [(_ (g ...) ...) #'(disj+ (conj+ g ...) ...)]))

(define (pull stream)
  (define s (stream-realize stream))
  (if (stream-immature? s)
      (pull s)
      s))

(define (take-all stream)
  (define s (pull stream))
  (if (stream-empty? s)
      mzero
      (stream-add (stream-first s) (take-all (stream-rest s)))))

(define (take n stream)
  (if (zero? n)
      mzero
      (let [(s (pull stream))]
        (if (stream-empty? s)
            mzero
            (stream-add (stream-first s) (take (sub1 n) (stream-rest s)))))))

(define-syntax (run stx)
  (syntax-parse stx
    [(_ n (x ...) g0 g ...) #'(miniKanren-reify
                               (take n (call/empty-state
                                        (fresh (x ...)
                                               g0 g ...))))]))

(define-syntax (run* stx)
  (syntax-parse stx
    [(_ n (x ...) g0 g ...) #'(miniKanren-reify
                               (take-all (call/empty-state
                                          (fresh (x ...)
                                                 g0 g ...))))]))

;; Something wrong here...
(define (miniKanren-reify states)
  (map reify-state/1st-var states))

(define (reify-state/1st-var state)
  (define v (walk* (var 0) state))
  (walk* v (reify-s v empty-state)))

(define (walk* u s)
  (define v (walk u s))
  (cond
    [(var? v) v]
    [(binding? v) (cons (walk* (binding-var v) s)
                        (walk* (binding-value v) s))]
    [else v]))

(define (reify-name v)
  (string->symbol (string-append "_" "." (number->string (var-id v)))))

(define (reify-s u s)
  (define v (walk u s))
  (cond
    [(var? v) (define n (reify-name v))
              (extend-state s n v)]
    [(binding? v) (reify-s (binding-value v) (reify-s (binding-var v) s))]
    [else s]))
