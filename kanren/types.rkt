#lang racket/base

;; The idea of this file is to provide everything implementation specifiv for vars, bindings and the state,
;; so these can be switched out and tinkered with if needed

(provide (all-defined-out))

(require racket/match)

;;;; Vars
(struct LVar (id) #:transparent)

(define var? LVar?)

(define (var id)
  (LVar id))

(define var=? equal?)
(define var-id LVar-id)

;;;; Bindings
(define empty-bindings (list))

(define (extend-bindings bindings x v)
  (cons (cons x v) bindings))

(define (lookup-binding bindings x)
  (assoc x bindings))

(define binding? pair?)
(define binding-var car)
(define binding-value cdr)

;;;; State
(struct KanrenState (bindings next-id) #:transparent)

(define empty-state (KanrenState empty-bindings 0))

(define (extend-state state x v)
  (KanrenState (extend-bindings (KanrenState-bindings state) x v)
               (KanrenState-next-id state)))

(define (fresh-var state)
  (values (var (KanrenState-next-id state))
          (KanrenState (KanrenState-bindings state)
                       (add1 (KanrenState-next-id state)))))

(define (lookup-state s x)
  (lookup-binding (KanrenState-bindings s) x))

;;;; Streams
(define (unit s)
  (stream-add s mzero))

(define mzero '())
(define stream-empty? null?)
(define stream-immature? procedure?)
(define (stream-realize stream)
  (if (stream-immature? stream)
      (stream)
      stream))
(define stream-first car)
(define stream-rest cdr)
(define stream-add cons)
