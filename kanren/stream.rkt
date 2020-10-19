#lang racket/base

(require "types.rkt")

(provide (all-defined-out))

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
