#lang racket/base
;; All pure chess-piece logic goes here
(require racket/class
         racket/list
         racket/string)
(provide chess-piece-mixin
         chess-board-mixin
         location->rank-file
         rank-file->location
         fen-string->pieces
         chess-piece-data
         initial-fen
         initial-pieces
         setup-board
         chess960-fen-string
         chess960-string?)

(define (chess-piece-mixin %)
  (class* % (printable<%>)
    (init-field name glyph moves [location #f])
    (super-new)

    (define/public (get-colour)
      (if (equal? (string-upcase name) name)
          'white
          'black))

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)

    (define/public (get-glyph) glyph)

    (define/public (get-name) name)

    (define/public (valid-moves board)
      (if location
          (moves board location)
          '()))

    (define/public (custom-print port quoting-depth)
      (print (format "~A" glyph) port))
    (define/public (custom-write port)
      (write (format "~A" glyph) port))
    (define/public (custom-display port)
      (display (format "~A" glyph) port))))

(define (chess-board-mixin %)
  (define new-% (if (method-in-interface? 'insert (class->interface %))
                    %
                    (class %
                      (super-new)
                      (define/pubment (insert . rest)
                        (inner (void) after-insert . rest))
                      (define/pubment (after-insert . rest)
                        (void)))))
  (class* new-% (printable<%>)
    (super-new)

    (init-field [turn 'white])

    (define pieces '())

    (define/public (end-turn)
      (set! turn (if (eq? turn 'white) 'black 'white)))

    (define/augment (after-insert chess-piece . args)
      (set! pieces (cons chess-piece pieces)))

    (define/public (get-pieces)
      pieces)
    (define/public (get-turn)
      turn)

    (define/public (piece-at-location location)
      (let ([ps (filter (lambda (p) (equal? (send p get-location) location))
                        pieces)])
        (if (empty? ps)
            #f
            (first ps))))

    (define/public (collect-moves colour)
      (define opponent-pieces (filter (lambda (p) (equal? colour (send p get-colour)))
                                      (send this get-pieces)))
      (remove-duplicates
       (for/fold ([moves '()])
                 ([piece (in-list opponent-pieces)])
         (append (send piece valid-moves this) moves))))

    (define/public (can-move-to location [colour turn])
      (define can-move-to-location? (lambda (piece)
                                      (and (if colour
                                               (equal? colour (send piece get-colour))
                                               #t)
                                           (member location (send piece valid-moves this)))))
      (filter can-move-to-location? pieces))

    (define/public (move-piece-to piece location)
      (define valid-moves (send piece valid-moves this))
      (if (member location valid-moves)
          (let ([target-piece (send this piece-at-location location)])
            (when (and target-piece
                       (not (eq? target-piece piece)))
              (send target-piece set-location #f))
            ;; Update the piece position *after* possibly removing the old piece
            (send piece set-location location)
            (send this end-turn)
            target-piece)
          #f))

    (define (turn-name name)
      (if (eq? turn 'white)
          (string-upcase name)
          (string-downcase name)))

    (define/public (make-move algebraic-notation)
      (define len          (string-length algebraic-notation))
      (define location     (substring algebraic-notation (- len 2) len))
      (define non-location (substring algebraic-notation 0 (- len 2)))
      (define piece-name   (if (or (= (string-length non-location) 0)
                                   (member (string-ref non-location 0)
                                           '(#\a #\b #\c #\d #\e #\f #\g #\h)))
                               (turn-name "p")
                               (string (string-ref non-location 0))))
      (define pred?        (lambda (piece)
                             (and (member location (send piece valid-moves this))
                                  (equal? piece-name (send piece get-name)))))
      (define pieces       (filter pred? (send this can-move-to location)))
      (when (= (length pieces) 1)
        (send this move-piece-to (car pieces) location)))

    (define/public (make-moves moves)
      (define old-turn 'undefined)
      (for/list ([move (in-list moves)])
        (set! old-turn turn)
        (send this make-move move)
        #:break (equal? old-turn turn)
        move))

    (define black-square #\u25A0)
    (define white-square #\u25A1)

    ;; These should probably be improved
    (define/public (custom-print port quoting-depth)
      (write-string "#<chess-board>" port))
    (define/public (custom-write port)
      (write-string "#<chess-board>") port)
    (define/public (custom-display port)
      (for ([rank (in-range 8)])
        (for ([file (in-range 8)])
          (define location (rank-file->location rank file))
          (define piece (send this piece-at-location location))
          (display (if piece
                       (send piece get-glyph)
                       (if (or (and (odd? rank)  (even? file))
                               (and (even? rank) (odd? file)))
                           black-square
                           white-square))
                   port))
        (display "\n" port)))))

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
  (unless (<= 0 rank 7)
    (raise-argument-error 'rank "integer between 0 and 7" rank))
  (unless (<= 0 file 7)
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

(define (setup-board board fen-string chess-piece-constructor)
  (for ([piece (in-list (fen-string->pieces fen-string))])
    (send board insert (apply chess-piece-constructor piece))))

(define (chess960-list? l)
  (define king (index-of l #\k))
  (define rooks (indexes-of l #\r))
  (define bishops (indexes-of l #\b))
  (and (< (first rooks) king (second rooks))
       (not (= (modulo (first bishops) 2) (modulo (second bishops) 2)))))

(define (chess960-string? s)
  (chess960-list? (string->list s)))

(define (chess960-fen-string)
  (define all-possibilities (filter chess960-list?
                                    (permutations '(#\r #\n #\b #\q #\k #\b #\n #\r))))
  (define piece-string (list->string (list-ref all-possibilities (random (length all-possibilities)))))
  (define pawn-string "pppppppp")
  (string-join (list piece-string
                     pawn-string
                     "8"
                     "8"
                     "8"
                     "8"
                     (string-upcase pawn-string)
                     (string-upcase piece-string))
               "/"))

(define initial-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
(define initial-pieces (fen-string->pieces initial-fen))

(define (valid-rank? rank) (and (>= rank 0) (< rank 8)))
(define (valid-file? file) (and (>= file 0) (< file 8)))

(define ((pawn-moves colour) board location)
  (define direction (if (eq? colour 'white) -1 1))
  (define-values (rank file) (location->rank-file location))
  (define moves '())
  (when (valid-rank? (+ rank direction))
    ;; can move forward if that square is not occupied
    (let ((candidate (rank-file->location (+ rank direction) file)))
      (unless (send board piece-at-location candidate)
        (set! moves (cons candidate moves))
        (when (valid-rank? (+ rank direction direction))
          ;; can move two squares forward if the pawn is in its original location
          (when (or (and (eq? colour 'white) (equal? rank 6))
                    (and (eq? colour 'black) (equal? rank 1)))
            (let ((candidate (rank-file->location (+ rank direction direction) file)))
              (unless (send board piece-at-location candidate)
                (set! moves (cons candidate moves))))))))
    ;; can move forward left if that square is occupied
    (when (valid-file? (sub1 file))
      (let ((candidate (rank-file->location (+ rank direction) (sub1 file))))
        (let ((piece (send board piece-at-location candidate)))
          (when (and piece (not (eq? colour (send piece get-colour))))
            (set! moves (cons candidate moves))))))
    ;; can move forward right if that square is occupied
    (when (valid-file? (add1 file))
      (let ((candidate (rank-file->location (+ rank direction) (add1 file))))
        (let ((piece (send board piece-at-location candidate)))
          (when (and piece (not (eq? colour (send piece get-colour))))
            (set! moves (cons candidate moves)))))))

  moves)

(define (valid-moves-by-offset colour board location offsets)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves '()])
            ([offset (in-list offsets)])
    (define-values (rank-offset file-offset) (apply values offset))
    (define-values (nrank nfile) (values (+ rank rank-offset) (+ file file-offset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (send board piece-at-location candidate)))
            (if (or (not piece) (not (eq? (send piece get-colour) colour)))
                (cons candidate moves)
                moves)))
        moves)))

(define (valid-moves-by-direction colour board location rank-direction file-direction)
  (define-values (rank file) (location->rank-file location))
  (define moves '())
  (define (check rank file)
    (let ((candidate (rank-file->location rank file)))
      (let ((target-piece (send board piece-at-location candidate)))
        (when (or (not target-piece) (not (eq? (send target-piece get-colour) colour)))
          (set! moves (cons candidate moves)))
        (if target-piece #f #t))))
  (let loop ((nrank (+ rank rank-direction))
             (nfile (+ file file-direction)))
    (when (and (valid-rank? nrank) (valid-file? nfile) (check nrank nfile))
      (loop (+ nrank rank-direction) (+ nfile file-direction))))
  moves)

(define ((knight-moves colour) board location)
  (valid-moves-by-offset
   colour board location
   '((-1 -2) (-1 2) (1 -2) (1 2) (-2 -1) (-2 1) (2 -1) (2 1))))

(define ((king-moves colour) board location)
  (valid-moves-by-offset
   colour board location
   '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))

(define ((rook-moves colour) board location)
  (append
   (valid-moves-by-direction colour board location 1 0)
   (valid-moves-by-direction colour board location -1 0)
   (valid-moves-by-direction colour board location 0 1)
   (valid-moves-by-direction colour board location 0 -1)))

(define ((bishop-moves colour) board location)
  (append
   (valid-moves-by-direction colour board location 1 1)
   (valid-moves-by-direction colour board location -1 1)
   (valid-moves-by-direction colour board location 1 -1)
   (valid-moves-by-direction colour board location -1 -1)))

(define ((queen-moves colour) board location)
  (append
   (valid-moves-by-direction colour board location 1 0)
   (valid-moves-by-direction colour board location -1 0)
   (valid-moves-by-direction colour board location 0 1)
   (valid-moves-by-direction colour board location 0 -1)
   (valid-moves-by-direction colour board location 1 1)
   (valid-moves-by-direction colour board location -1 1)
   (valid-moves-by-direction colour board location 1 -1)
   (valid-moves-by-direction colour board location -1 -1)))

(define chess-piece-data
  (hash
   "K" (cons #\u2654 (king-moves 'white))
   "Q" (cons #\u2655 (queen-moves 'white))
   "R" (cons #\u2656 (rook-moves 'white))
   "B" (cons #\u2657 (bishop-moves 'white))
   "N" (cons #\u2658 (knight-moves 'white))
   "P" (cons #\u2659 (pawn-moves 'white))
   "k" (cons #\u265A (king-moves 'black))
   "q" (cons #\u265B (queen-moves 'black))
   "r" (cons #\u265C (rook-moves 'black))
   "b" (cons #\u265D (bishop-moves 'black))
   "n" (cons #\u265E (knight-moves 'black))
   "p" (cons #\u265F (pawn-moves 'black))))
