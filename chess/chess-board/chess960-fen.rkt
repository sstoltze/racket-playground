#lang racket/base
;; To create an exectuable, run
;; raco exe chess960-fen.rkt
(require chess-board)

(printf "~A" (chess960-fen-string))
