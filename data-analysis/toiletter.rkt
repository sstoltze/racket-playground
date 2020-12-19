#lang racket
(require plot
         net/url
         json
         "geojson.rkt")
(provide toiletter-renderers
         draw-toiletter)

(define by-toiletter-url (string->url "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=by_toiletter"))
(define andre-toiletter-url (string->url "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=andre_toiletter"))
(define url->json (compose read-json get-pure-port))
(define by-toiletter-geojson (url->json by-toiletter-url))
(define andre-toiletter-geojson (url->json andre-toiletter-url))

(define (toiletter-renderers)
  (append (make-point-renderers by-toiletter-geojson)
          (make-point-renderers andre-toiletter-geojson)))

(define (draw-toiletter)
  (plot (toiletter-renderers)))
