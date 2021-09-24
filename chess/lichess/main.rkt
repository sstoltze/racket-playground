#lang racket/base
(require net/url
         json
         racket/file
         chess-board/simple)

(define base-url (string->url "https://lichess.org"))
(define token (file->string ".token"))
(define header (list (format "Authorization: Bearer ~A" token)))

(define (lichess-url [path #f])
  (combine-url/relative base-url
                        (apply string-append "/api/account" (if path
                                                                (list "/" path)
                                                                '("")))))

(define (lichess-json lichess-url)
  (read-json (get-pure-port lichess-url
                            header)))

(define (ongoing-games)
  (hash-ref (lichess-json (lichess-url "playing"))
            'nowPlaying))

(define (my-turn? game)
  (hash-ref game 'isMyTurn))

(define (game-fen game)
  (hash-ref game 'fen))

(define (print-game game)
  (printf "~A~%" (make-chess-board (game-fen game))))
