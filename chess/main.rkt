#lang racket

(require racket/gui
         embedded-gui)

;; Snips need a class
(define chess-piece-snip-class
  (make-object (class snip-class%
                 (super-new)
                 ;; Classnames should be unique
                 (send this set-classname "chess-piece-snip"))))
(send (get-the-snip-class-list) add chess-piece-snip-class)

(define chess-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    ;; Defines the size of the snip
    (define/override (get-extent dc x y width height descent space lspace rspace)
      ;; We only care about width and height
      ;; get-extent uses boxes for the output values
      (when width (set-box! width size))
      (when height (set-box! height size))
      ;; The rest are not important, set them to zero
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ([ox (/ (- size glyph-width) 2)]
            [oy (/ (- size glyph-height) 2)])
        (send dc draw-text glyph (+ x ox) (+ y oy))))

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)))

(define chess-board%
  (class pasteboard%
    (super-new)
    ;; When redrawing, draw black squares before adding snips
    (define/override (on-paint before? dc . _)
      (when before?
        (draw-chess-board dc)))

    ;; After a piece is added, position it correctly
    (define/augment (after-insert chess-piece . _)
      (position-piece this chess-piece))

    ;; Redraw board when resized
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))))

(define chess-piece-glyphs
  (hash
   "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
   "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

(define initial-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

(define (make-chess-piece id [location #f])
  (define glyph (hash-ref chess-piece-glyphs id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece% [glyph (string glyph)] [font font] [size 35] [location location]))

(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 8))
  (define cell-height (/ dc-height 8))
  (define margin 3)

  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)

  ;; Draw black squares
  (for* ([row (in-range 8)]
         [col (in-range 8)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values (x y) (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  ;; Draw rank and file on the board
  (for ([(rank index) (in-indexed '("8" "7" "6" "5" "4" "3" "2" "1"))])
    (define-values (_0 h _1 _2) (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
    (define-values (w h _1 _2) (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ 2 2))))
    (send dc draw-text file x (- dc-height h margin))))

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
                     (cons (make-chess-piece (format "~A" c)
                                             (format "~A~A" (number->file file) rank))
                           pieces))])))
  (for/fold ([pieces '()])
            ([(row rank) (in-indexed (string-split fen "/"))])
    ;; FEN starts at the top of the board, so the first line is rank 8
    (append (fen-row->pieces (- 8 rank) row)
            pieces)))

;; Positions a piece on the correct square based on its location
(define (position-piece board piece)
  (when (send piece get-location)
    (define-values (canvas-width canvas-height)
      (let ([c (send board get-canvas)])
        (send c get-size)))
    (define-values (square-width square-height)
      (values (/ canvas-width 8) (/ canvas-height 8)))
    (define-values (rank file)
      (location->rank-file (send piece get-location)))
    (define-values (square-x square-y)
      (values (* file square-width) (* rank square-height)))
    (define piece-width (snip-width piece))
    (define piece-height (snip-height piece))
    (send board move-to piece
          (+ square-x (/ (- square-width piece-width) 2))
          (+ square-y (/ (- square-height piece-height) 2)))))

(define (setup-board board fen-string)
  (send board clear)
  (for ([piece (in-list (fen-string->pieces fen-string))])
    (send board insert piece)))

(define board (new chess-board%))
(define frame (new frame%
                   [label "Chess Board"]
                   [width (* 50 8)]
                   [height (* 50 8)]))
(define canvas (new editor-canvas%
                    [parent frame]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
(setup-board board initial-fen)
(send frame show #t)
