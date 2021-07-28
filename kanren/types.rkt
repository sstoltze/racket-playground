#lang racket/base
;; The idea of this file is to provide everything implementation specifiv for vars, bindings and the state,
;; so these can be switched out and tinkered with if needed

;;;; Vars
(module kanren-var racket/base
  (provide (all-defined-out))
  (struct LVar (id) #:transparent)

  (define var? LVar?)

  (define (var id)
    (LVar id))

  (define var=? equal?)
  (define var-id LVar-id))

(require 'kanren-var)
(provide (all-from-out 'kanren-var))

;;;; Bindings
(module binding-alist racket/base
  (provide (all-defined-out))
  (define empty-bindings (list))

  (define (extend-bindings bindings x v)
    (cons (cons x v) bindings))

  (define (lookup-binding bindings x)
    (assoc x bindings))

  (define has-binding? assoc))

(module binding-hash racket/base
  (provide (all-defined-out))
  (define empty-bindings (hash))

  (define (extend-bindings bindings x v)
    (hash-set bindings x v))

  (define (lookup-binding bindings x)
    (when (has-binding? bindings x)
      (cons x (hash-ref bindings x))))

  (define has-binding? hash-has-key?))

(module kanren-binding racket/base
  (provide (all-defined-out))
  (require (submod ".." binding-hash))
  (provide (all-from-out (submod ".." binding-hash)))
  #;(require 'binding-alist)
  #;(provide (all-from-out 'binding-alist))

  ;; A binding is a cons pair
  (define binding? pair?)
  (define binding-var car)
  (define binding-value cdr))

(require 'kanren-binding)
(provide (all-from-out 'kanren-binding))

;;;; State
(module kanren-state racket/base
  (provide (all-defined-out))
  (require (submod ".." kanren-binding))
  (require (submod ".." kanren-var))
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

  (define (state-vars s)
    (for/list [(i (in-range (KanrenState-next-id s)))]
      (var i))))

(require 'kanren-state)
(provide (all-from-out 'kanren-state))

;;;; Streams
(module kanren-stream racket/base
  (provide (all-defined-out))
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
  (define stream-add cons))

(require 'kanren-stream)
(provide (all-from-out 'kanren-stream))
