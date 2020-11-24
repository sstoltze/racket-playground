#lang racket
(require data-frame
         db
         racket/draw
         racket/gui
         plot
         "database.rkt"
         "traffic.rkt")

(plot-new-window? #t)

;; (df-describe df)
(define df (df-read/sql pgc "select * from full_traffic order by timestamp"))

(define (describe-report-ids)
  (map (compose (curry apply format "~a: From ~a, ~a ~a to ~a, ~a ~a")
                vector->list)
       (vector->list (df-select* df "report_id" "point_1_name" "point_1_street" "point_1_street_number" "point_2_name" "point_2_street" "point_2_street_number"))))

(define (plot-stacked-count v id)
  (define danger-color (send the-color-database find-color "Red"))
  (define all-ok-color (send the-color-database find-color "SeaGreen"))
  (parameterize ([plot-width               1200]
                 [plot-height              800]
                 [plot-x-label             (format "Vehicle count for '~a' by hour"
                                                   (lookup-from-id "report_name" id))]
                 [plot-x-ticks             (date-ticks)]
                 [plot-x-tick-label-angle  -60]
                 [plot-x-tick-label-anchor 'top-left]
                 [plot-y-label             ""]
                 [stacked-histogram-colors (list all-ok-color danger-color)])
    (plot (stacked-histogram v
                             #:labels '(#f "Speeding"))
          #:legend-anchor 'top-right)))

(define (show-full-vehicle-count report-id)
  (define vehicles-for-id
    (vector-map
     (lambda (v)
       (let ([speeding? (vector-ref v 3)]
             [time      (sql-timestamp->string (vector-ref v 1))]
             [count     (vector-ref v 2)])
         (vector time
                 (if speeding?
                     (list 0 count)
                     (list count 0)))))
     (get-vehicle-count-by-id report-id)))
  (plot-stacked-count vehicles-for-id report-id))

(define (get-vehicle-count-by-id report-id)
  (df-select* df
              #:filter (lambda (v) (= (vector-ref v 0)
                                      report-id))
              "report_id"
              "timestamp"
              "vehicle_count"
              "speeding"))

(define (show-hourly-vehicle-count report-id)
  (define time-hash (make-hash))
  (for ([v (in-vector (get-vehicle-count-by-id report-id))])
    (let ([speeding? (vector-ref v 3)]
          [time      (vector-ref v 1)]
          [count     (vector-ref v 2)])
      (match time
        [(sql-timestamp y mo d h _ _ _ _)
         (define prev (hash-ref time-hash h (list 0 0)))
         (hash-set! time-hash h (if speeding?
                                    (list (first prev)
                                          (+ (second prev) count))
                                    (list (+ (first prev) count)
                                          (second prev))))])))
  (plot-stacked-count (vector-sort
                       (for/vector ([(k v) (in-hash time-hash)])
                         (vector k v))
                       <=
                       #:key (curryr vector-ref 0))
                      report-id))


;;(define-values (data     data-next)     (get-records-from data-start))
;;(define-values (metadata metadata-next) (get-records-from metadata-start))
