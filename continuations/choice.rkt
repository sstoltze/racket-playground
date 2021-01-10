#lang racket/base
(provide choose fail assert)
(define (current-continuation)
  (call/cc (lambda (cc) cc)))

;; Non-deterministic choice. Select from a series of alternatives and use a 'fail' call to signify a wrong choice
;; From https://www.it.uu.se/edu/course/homepage/avfunpro/ht13/lectures/Racket-2-Continuations.pdf
;; A simple example to illustrate the idea
(define (simple-choose a b)
  (let ([cc (current-continuation)])
    (cond [(continuation? cc) (values a cc)]
          [else (values b #f)])))

(define (simple-fail cc)
  (when (continuation? cc) (cc #f)))

(define (simple-run)
  (define-values (val cc) (simple-choose 3 4))
  (displayln val)
  (when (= val 3) (simple-fail cc))
  (displayln val))

;; A more general example that avoids passing the continuation around
;; A stack of possible choices
(define cc-stack '())

;; Make a continutation for each value and store them on the stack
(define (choose a . rest)
  (let ([cc (current-continuation)])
    (cond [(null? rest)       a]
          [(continuation? cc) (set! cc-stack (cons cc cc-stack))
                              a]
          [else               (apply choose rest)])))

;; Signal that the current choice of value is bad
(define (fail)
  (cond [(null? cc-stack) (raise 'empty-stack)]
        [else (let ([cc (car cc-stack)])
                (set! cc-stack (cdr cc-stack))
                (cc #f))]))

(define (assert predicate)
  (when (not predicate) (fail)))

(define (run)
  (let ([a (choose 1 2 3 4 5 6 7 8 9)]
        [b (choose 1 2 3 4 5 6 7 8 9)]
        [c (choose 1 2 3 4 5 6 7 8 9)])
    (assert (= (+ (* a a) (* b b)) (* c c)))
    (displayln (list a b c))))
