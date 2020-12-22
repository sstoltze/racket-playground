#lang racket
(require "chess.rkt")

(define simple-chess-piece% (chess-piece-mixin object%))
(define simple-chess-board% (chess-board-mixin object%))

(define (make-simple-chess-piece id [location #f])
  (match-define (cons glyph moves) (hash-ref chess-piece-data id))
  (new simple-chess-piece%
       [name id]
       [glyph glyph]
       [location location]
       [moves moves]))

(define (make-simple-chess-board [fen-string #f])
  (define board (new simple-chess-board%))
  (when fen-string
    (setup-board board fen-string make-simple-chess-piece))
  board)

(define board (make-simple-chess-board initial-fen))
