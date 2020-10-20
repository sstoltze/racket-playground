#lang racket/base

(require "types.rkt"
         "base.rkt")

(provide (all-defined-out))

(define (miniKanren-reify states)
  (map reify-state/1st-var states))

(define (reify-state/1st-var state)
  (reify-state/var (var 0) state))

(define (walk* u s)
  (define v (walk u s))
  (cond
    [(var? v) v]
    [(pair? v) (cons (walk* (car v) s)
                     (walk* (cdr v) s))]
    [else v]))

(define (reify-name v)
  (string->symbol (string-append "_." (number->string (var-id v)))))

(define (reify-s u s)
  (define v (walk u s))
  (cond
    [(var? v) (define n (reify-name v))
              (extend-state s v n)]
    [(pair? v) (reify-s (car v) (reify-s (cdr v) s))]
    [else s]))

(define (reify-all states)
  (map reify-state/all states))

(define (reify-state/var u state)
  (define v (walk* u state))
  (walk* v (reify-s v empty-state)))

(define (reify-state/all s)
  (for/list [(v (in-list (state-vars s)))]
    (list (reify-name v) (reify-state/var v s))))

(define (reify-format/all f states)
  (map (lambda (vars)
         (map (lambda (v) (list (car v) (f (cadr v))))
              vars))
       states))
