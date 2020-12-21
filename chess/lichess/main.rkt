#lang racket
(require net/url
         json)

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
