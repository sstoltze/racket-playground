#lang racket
(require racket/gui
         embedded-gui
         "main.rkt")
(provide (all-from-out "main.rkt")
         make-chess-board
         make-chess-piece)

;; Snips need a class
(define chess-piece-snip-class
  (make-object (class snip-class%
                 (super-new)
                 ;; Classnames should be unique
                 (send this set-classname "chess-piece-snip"))))
(send (get-the-snip-class-list) add chess-piece-snip-class)

(define chess-piece-snip%
  (class (chess-piece-mixin snip%)
    (init-field font size)
    (inherit-field glyph)
    (super-new)

    (send this set-snipclass chess-piece-snip-class)

    ;; The pieces will keep track of when they are selected and change how they are drawn
    (define selected? #f)

    (define/public (set-selected on?)
      (set! selected? on?)
      ;; Tell the current administrator that this snip needs to be redrawn
      (let ([admin (send this get-admin)])
        (when admin
          (send admin needs-update this 0 0 size size))))

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
      (if selected?
          (send dc set-text-foreground "red")
          (send dc set-text-foreground "black"))
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ([dx (/ (- size glyph-width) 2)]
            [dy (/ (- size glyph-height) 2)])
        (send dc draw-text glyph (+ x dx) (+ y dy))))))

(define chess-board%
  (class (chess-board-mixin pasteboard%)
    (super-new)
    (inherit-field turn)

    ;; Disable drag-select
    (send this set-area-selectable #f)

    (send this set-selection-visible #f)

    ;; Used for highlighting the location under the cursor when dragging a piece
    (define highlight-location #f)
    (define drag-dx 0)
    (define drag-dy 0)

    ;; Locations to highlight
    (define valid-move-locations '())
    (define opponent-move-locations '())

    (define/augment (can-interactive-move? event)
      (define piece (send this find-next-selected-snip #f))
      (unless (eq? turn (send piece get-colour))
        (set-message (format "It's ~a turn to move"
                             (if (eq? turn 'white) "white's" "black's"))))
      (eq? turn (send piece get-colour)))

    ;; When redrawing, draw black squares before adding snips
    (define/override (on-paint before? dc . _)
      (if before?
          (begin
            (draw-chess-board dc)
            (for ([location (in-list valid-move-locations)])
              (highlight-square dc location #f "seagreen"))
            (for ([location (in-list opponent-move-locations)])
              (highlight-square dc location "firebrick" #f))
            (when highlight-location
              (highlight-square dc highlight-location #f "indianred")))
          (when message
            (display-message dc message))))

    ;; After a piece is added, position it correctly
    (define/augment (after-insert chess-piece . _)
      (position-piece this chess-piece))

    ;; Redraw board when resized
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (for ([piece (in-list (send this get-pieces))])
        (position-piece this piece))
      (send this end-edit-sequence))

    ;; No editing
    (define/override (can-do-edit-operation? op recursive?)
      #f)

    (define/augment (after-select snip on?)
      (send snip set-selected on?)
      (if on?
          (begin
            (unless (eq? turn (send snip get-colour))
              (set-message (format "It's ~a turn to move"
                                   (if (eq? turn 'white) "white's" "black's"))))
            ;; Move the snip to the front when selecting, so it is kept on top
            (send this set-before snip #f)
            ;; If more than one snip is selected, only keep one
            (let ([other-selected-snips
                   (let loop ([other (send this find-next-selected-snip #f)]
                              [result '()])
                     (if other
                         (let ([next (send this find-next-selected-snip other)])
                           (if (equal? snip other)
                               (loop next result)
                               (loop next (cons other result))))
                         result))])
              (for ([snip other-selected-snips])
                (send this remove-selected snip)))
            (set! valid-move-locations (send snip valid-moves this))
            (set! opponent-move-locations (send this collect-moves
                                                (if (eq? (send snip get-colour) 'white)
                                                    'black
                                                    'white))))
          (begin
            (set! valid-move-locations '())
            (set! opponent-move-locations '()))))

    (define/augment (on-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define-values (x y) (values (box 0) (box 0)))
      (send this get-snip-location piece x y #f)
      (set! drag-dx (- (send event get-x) (unbox x)))
      (set! drag-dy (- (send event get-y) (unbox y))))

    (define/augment (on-move-to snip x y dragging?)
      (when dragging?
        (let ([location (xy->location this (+ x drag-dx) (+ y drag-dy))])
          (unless (equal? highlight-location location)
            (set! highlight-location location)
            (send (send this get-canvas) refresh)))))

    (define/augment (after-interactive-move event)
      (set! highlight-location #f)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (define valid-moves (send piece valid-moves this))
      (when (member location valid-moves)
        (let ([target-piece (send this piece-at-location location)])
          (when (and target-piece
                     (not (eq? target-piece piece)))
            (send target-piece set-location #f)
            (send this remove target-piece)))
        ;; Update the piece position *after* possibly removing the old piece
        (send piece set-location location)
        (send this end-turn))
      (position-piece this piece)
      (set! valid-move-locations (send piece valid-moves this))
      (send (send this get-canvas) refresh))

    ;; Disable keys by creating a new keymap
    (define (on-disabled-key-event data event)
      (if (is-a? event key-event%)
          (let* ([code (send event get-key-code)]
                 [key-name (cond ((symbol? code) (symbol->string code))
                                 ((equal? code #\backspace) "backspace")
                                 ((equal? code #\rubout) "delete")
                                 ((equal? code #\space) "space")
                                 ((equal? code #\return) "return")
                                 (#t (string code)))])
            (set-message (format "~a key is disabled" key-name)))
          (set-message "event is discarded")))

    (define k (new keymap%))
    (send k add-function "ignore" on-disabled-key-event)
    (send k map-function "up" "ignore")
    (send k map-function "down" "ignore")
    (send k map-function "left" "ignore")
    (send k map-function "right" "ignore")
    (send k map-function "del" "ignore")
    (send k map-function "backspace" "ignore")
    ;; Instead of setting a keymap, you could override the on-char method
    ;; But this is apparently hard to do correctly, so this is the easy solution
    (send this set-keymap k)

    ;; Do not insert a new chess-piece if it is not valid
    ;; i.e. not a chess-piece, does not have a location set or the space is occupied
    (define/augment (can-insert? snip . rest)
      (and (is-a? snip chess-piece-snip%)
           (send snip get-location)
           (not (send this piece-at-location (send snip get-location)))))

    (define message #f)
    (define message-timer (new timer%
                               [notify-callback (lambda ()
                                                  (set! message #f)
                                                  (send (send this get-canvas) refresh))]))

    (define (set-message m)
      (set! message m)
      (send message-timer start 2000)
      (send (send this get-canvas) refresh))))

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
         #:when (or (and (odd? row)  (even? col))
                    (and (even? row) (odd? col))))
    (define-values (x y) (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  ;; Draw rank and file on the board
  (for ([(rank index) (in-indexed '("8" "7" "6" "5" "4" "3" "2" "1"))])
    (define-values (_0 h _1 _2)
      (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height)
                 (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
    (define-values (w h _1 _2)
      (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width)
                 (- (/ cell-width 2) (/ 2 2))))
    (send dc draw-text file x (- dc-height h margin))))

(define (display-message dc message)
  (define font (send the-font-list find-or-create-font 24 'default 'normal 'normal))
  (define-values (w h _1 _2) (send dc get-text-extent message font #t))
  (define-values (dc-width dc-height) (send dc get-size))
  (define-values (x y) (values (/ (- dc-width w) 2) (/ (- dc-height h) 2)))

  (define brush (send the-brush-list find-or-create-brush "bisque" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc draw-rectangle 0 y dc-width h)
  (send dc set-font font)
  (send dc set-text-foreground "firebrick")
  (send dc draw-text message x y))

(define (highlight-square dc location colour-name border-colour-name)
  (define-values (rank file) (location->rank-file location))
  (define brush
    (if colour-name
        (let* ([base (send the-color-database find-color colour-name)]
               [colour (make-object color% (send base red) (send base green) (send base blue) 0.3)])
          (send the-brush-list find-or-create-brush colour 'solid))
        (send the-brush-list find-or-create-brush "black" 'transparent)))
  (define pen
    (if border-colour-name
        (send the-pen-list find-or-create-pen border-colour-name 2 'solid)
        (send the-pen-list find-or-create-pen "black" 1 'transparent)))
  (send dc set-pen pen)
  (send dc set-brush brush)
  (define-values (dc-width dc-height)
    (send dc get-size))
  (define-values (cell-width cell-height)
    (values (/ dc-width 8) (/ dc-height 8)))
  (send dc draw-rectangle (* file cell-width) (* rank cell-height) cell-width cell-height))

(define (xy->location board x y)
  (define-values (canvas-width canvas-height)
    (let ([c (send board get-canvas)])
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 8) (/ canvas-height 8)))
  (define-values (rank file)
    (values (exact-truncate (/ y square-height)) (exact-truncate (/ x square-width))))
  (rank-file->location rank file))

;; Positions a piece on the correct square based on its location
(define (position-piece board piece)
  (when (and (send piece get-location)
             (send board get-canvas))
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

(define (make-chess-piece id [location #f])
  (match-define (cons glyph moves) (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece-snip%
       [name id]
       [glyph (string glyph)]
       [font font]
       [size 35]
       [moves moves]
       [location location]))

(define (make-chess-board [fen-string initial-fen])
  (define board (new chess-board%))
  (define frame (new frame%
                     [label  "Chess Board"]
                     [width  (* 50 8)]
                     [height (* 50 8)]))
  (define canvas (new editor-canvas%
                      [parent           frame]
                      [style            '(no-hscroll no-vscroll)]
                      [horizontal-inset 0]
                      [vertical-inset   0]
                      [editor           board]))
  (setup-board board initial-fen make-chess-piece)
  (send frame show #t)
  board)
