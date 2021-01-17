#lang racket/base
(require racket/class
         racket/match
         "main.rkt")
(provide (all-from-out "main.rkt")
         make-chess-piece
         make-chess-board
         make-chess960-board)

(define simple-chess-piece% (chess-piece-mixin object%))
(define simple-chess-board% (chess-board-mixin object%))

(define (make-chess-piece id [location #f])
  (match-define (cons glyph moves) (hash-ref chess-piece-data id))
  (new simple-chess-piece%
       [name id]
       [glyph glyph]
       [location location]
       [moves moves]))

(define (make-chess-board [fen-string initial-fen])
  (define board (new simple-chess-board%))
  (when fen-string
    (setup-board board fen-string make-chess-piece))
  board)

(define (make-chess960-board)
  (make-chess-board (chess960-fen-string)))
