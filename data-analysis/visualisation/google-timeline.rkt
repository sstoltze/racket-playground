#lang racket
(require json
         plot
         plot/utils)

(define full-location-history-path "Takeout/Location History/Location History.json")
;; (define location-history (call-with-input-file location-history-path read-json))

(define test-location-history-path "Takeout/Location History/Semantic Location History/2017/2017_JULY.json")
(define test-location-history (call-with-input-file test-location-history-path read-json))

(define timeline-objects (hash-ref test-location-history 'timelineObjects))
(define test-object (first timeline-objects))
(define (activity obj)
  (hash-ref obj 'activitySegment))
(define test-activity (activity test-object))

(define (activity-duration-ms activity)
  (define duration (hash-ref activity 'duration))
  (- (string->number (hash-ref duration 'endTimestampMs))
     (string->number (hash-ref duration 'startTimestampMs))))

(define (activity-path activity)
  (define start-location (hash-ref activity 'startLocation))
  (define end-location (hash-ref activity 'endLocation))
  (define (location->point location)
    (list (/ (hash-ref location 'longitudeE7) 1e7)
          (/ (hash-ref location 'latitudeE7) 1e7)))
  (list (location->point start-location)
        (location->point end-location)))

(define (activity-segment? obj)
  (hash-has-key? obj 'activitySegment))

(define (object->path obj)
  (if (activity-segment? obj)
    (activity-path (activity obj))
    #f))

(define (objects->path objs)
  (filter-map object->path objs))

(define (point->unit-vector p)
  (3d-polar->3d-cartesian (degrees->radians (first p))
                          (degrees->radians (second p))
                          1))

(define (render-path objs)
  (map (lambda (points)
         (lines3d (map point->unit-vector points)))
       (objects->path objs)))

(define (plot-path p)
  (plot3d p))
