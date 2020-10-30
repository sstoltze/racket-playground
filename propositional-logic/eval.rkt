#lang racket/base

(provide eval-proposition)

(require racket/function
         racket/match
         racket/list)

(define (eval-proposition vars sentence)
  (define eval-with-vars (curry eval-proposition vars))
  (match sentence
    [(list 'sentence s)         (eval-with-vars s)]
    [(list 'biimplication a b)  (eval-biimplication (eval-with-vars a) (eval-with-vars b))]
    [(list 'implication a b)    (eval-implication (eval-with-vars a) (eval-with-vars b))]
    [(list 'disjunction as ...) (eval-or (map eval-with-vars as))]
    [(list 'conjunction as ...) (eval-and (map eval-with-vars as))]
    [(list 'negation a)         (not (eval-with-vars a))]
    [(list 'atom a)             (hash-ref vars a (string->symbol a))]))

(define (eval-and lst)
  (define filtered (remove* '(#t) lst))
  (cond [(empty? filtered)    #t]
        [(member #f filtered) #f]
        [else                 (cons 'conjunction filtered)]))

(define (eval-or lst)
  (define filtered (remove* '(#f) lst))
  (cond [(empty? filtered)    #f]
        [(member #t filtered) #t]
        [else                 (cons 'dijunction filtered)]))

(define (true? a)
  (equal? a #t))
(define (false? a)
  (equal? a #f))

(define (eval-biimplication a b)
  (eval-and (list (eval-implication a b)
                  (eval-implication b a))))

(define (eval-implication a b)
  (if (and (true? a)
           (false? b))
      #f
      (or (false? a)
          (true? b)
          (list 'disjunction
                (list 'negation a)
                b))))
