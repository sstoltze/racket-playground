#lang racket
(require plot
         racket/gui
         net/url
         json
         "geojson.rkt")
(provide statistikdistrikter-renderers
         draw-statistikdistrikter
         snip-statistikdistrikter)

(define url (string->url "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=statistikdistrikter_fkg"))
(define url->json (compose read-json get-pure-port))
(define statistikdistrikt-geojson (url->json url))

(define (statistikdistrikter-renderers)
  (make-line-renderers statistikdistrikt-geojson))

(define (draw-statistikdistrikter)
  (plot (statistikdistrikter-renderers)))

(define (snip-statistikdistrikter)
  (plot-snip (statistikdistrikter-renderers)))

(define (gui)
  (define frame (new frame%
                     [label "test"]))
  (define snip (snip-statistikdistrikter))
  (define pasteboard (new pasteboard%))
  (send pasteboard insert snip)
  (define editor (new editor-canvas%
                      [parent frame]
                      [editor pasteboard]))
  (send frame show #t)
  frame)
