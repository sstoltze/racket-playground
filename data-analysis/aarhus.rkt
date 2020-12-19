#lang racket
(require plot
         "statistikdistrikter.rkt"
         "toiletter.rkt"
         "traffic.rkt"
         "wifi.rkt")

(define kommune-renderers (append (toiletter-renderers)
                                  (statistikdistrikter-renderers)))

(define city-renderers (append (traffic-renderers (sample-traffic-metadata))
                               (wifi-renderers)))

(define (draw-aarhus)
  (plot kommune-renderers))

(define (draw-city)
  (plot city-renderers))
