#lang racket/base
;; To create an exectuable, run
;; raco exe print-chess.rkt
(require chess-board/simple
         racket/cmdline)

(define fen
  (command-line
   #:args (fen-string)
   fen-string))

(printf "~A" (make-chess-board fen))
