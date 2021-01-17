#lang racket/base
(require net/url
         json
         racket/file
         chess-board/simple)

(define token (file->string ".token"))
(define header (list (format "Authorization: Bearer ~A" token)))

(define (lichess-url [path #f])
  (combine-url/relative (string->url "https://lichess.org")
                        (apply string-append "/api/account" (if path
                                                                (list "/" path)
                                                                '("")))))

(define (ongoing-games)
  (hash-ref (read-json (get-pure-port (lichess-url "playing")
                                      header))
            'nowPlaying))

(define (my-turn? game)
  (hash-ref game 'isMyTurn))

(define (game-fen game)
  (hash-ref game 'fen))

(define (print-game game)
  (printf "~A~%" (make-chess-board (game-fen game))))
