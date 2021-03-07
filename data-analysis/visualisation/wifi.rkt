#lang racket
(require plot
         racket/runtime-path)

(provide wifi-renderers)

(define (wifi-csv-line->points l)
  (define fields (string-split l ","))
  (define len (length fields))
  (list (string->number (list-ref fields (- len 1)))
        (string->number (list-ref fields (- len 2)))))

(define-runtime-path wifi-csv "wifi.csv")

(define wifi-csv-points
  (call-with-input-file wifi-csv
    (lambda (s)
      (define content (port->string s))
      (define lines (rest (string-split content "\n")))
      (map wifi-csv-line->points lines))))

(define (wifi-renderers)
  (list (points wifi-csv-points)))

(define (draw-wifi)
  (plot (wifi-renderers)))
