#lang racket
(provide (all-defined-out))
;; All pure chess-piece logic goes here
(define (chess-piece-mixin %)
  (class %
    (init-field glyph [location #f])
    (super-new)

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)))

(define chess-piece-glyphs
  (hash
   "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
   "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

;; Translates a location ("a1", ..., "h8") into a (rank file) pair (both numbers 0, ... 7)
(define (location->rank-file location)
  (unless (and (string? location) (= (string-length location) 2))
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h) (string-ref location 0)))
  (define rank
    (index-of '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (values rank file))

(define (rank-file->location rank file)
  (unless (<= 0 rank 8)
    (raise-argument-error 'rank "integer between 0 and 7" rank))
  (unless (<= 0 file 8)
    (raise-argument-error 'rank "integer between 0 and 7" file))
  (string
   (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h) file)
   (list-ref '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) rank)))

;; Use FEN for the board instead of the suggested notation
(define (fen-string->pieces fen)
  (define (char->number c)
    (string->number (format "~A" c)))
  (define (number->file i)
    (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h) i))
  (define (fen-row->pieces rank row)
    (for/fold ([file 0]
               [pieces '()]
               #:result pieces)
              ([c (in-string row)])
      (cond [(char-numeric? c)
             ;; A number represents that many blank spaces
             (values (+ file (char->number c))
                     pieces)]
            [else
             ;; Otherwise, it's a piece glyph
             (values (+ file 1)
                     (cons (list (format "~A" c)
                                 (format "~A~A" (number->file file) rank))
                           pieces))])))
  (for/fold ([pieces '()])
            ([(row rank) (in-indexed (string-split fen "/"))])
    ;; FEN starts at the top of the board, so the first line is rank 8
    (append (fen-row->pieces (- 8 rank) row)
            pieces)))

(define initial-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
(define initial-pieces (fen-string->pieces initial-fen))
