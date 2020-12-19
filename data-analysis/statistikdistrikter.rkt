#lang racket
(require plot
         net/url
         json
         "geojson.rkt")
(provide statistikdistrikter-renderers
         draw-statistikdistrikter)

(define url (string->url "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=statistikdistrikter_fkg"))
(define url->json (compose read-json get-pure-port))
(define statistikdistrikt-geojson (url->json url))

(define (statistikdistrikter-renderers)
  (make-line-renderers statistikdistrikt-geojson))

(define (draw-statistikdistrikter)
  (plot (statistikdistrikter-renderers)))
