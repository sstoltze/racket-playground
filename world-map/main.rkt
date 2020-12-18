#lang racket/base

(require plot
         plot/utils
         json
         racket/math
         racket/match)

(define (lat-lng->unit-vector lat lng)
  (3d-polar->3d-cartesian (degrees->radians lng)
                          (degrees->radians lat)
                          1))

(define (meridian lng)
  (define points (for/list ([lat (in-range -90.0 90.0 5.0)])
                   (lat-lng->unit-vector lat lng)))
  (lines3d points
           #:alpha 0.3))

(define unit-sphere
  (polar3d (lambda _ 1.0)
           #:color "navajowhite"
           #:line-style 'transparent
           #:alpha 0.9))

(define world-data (call-with-input-file "./custom.geo.json" read-json))

(define (make-polygon-renderer polygons)
  (for/fold ([renderers '()])
            ([polygon (in-list polygons)]
             #:unless (null? polygon))
    (define points
      (for/list ([point (in-sequences (in-list polygon)
                                      (in-value (car polygon)))])
        (match-define (list lng lat _ ...) point)
        (lat-lng->unit-vector lat lng )))
    (cons (lines3d points) renderers)))

(define (make-renderers world-map-data)
  (for/fold ([renderers '()])
            ([feature (in-list (hash-ref world-map-data 'features))])
    (let* ([geometry (hash-ref feature 'geometry (lambda () (hash)))]
           [data     (hash-ref geometry 'coordinates (lambda () null))])
      (case (hash-ref geometry 'type #f)
        [("Polygon") (cons (make-polygon-renderer data) renderers)]
        [("MultiPolygon") (cons (for/list ([polygon (in-list data)])
                                  (make-polygon-renderer polygon))
                                renderers)]
        [else (printf "Skipping ~A geometry" (hash-ref geometry 'type #f))
              renderers]))))

(define (draw-globe [angle 270] [altitude 20])
  (plot3d (cons unit-sphere
                (append (for/list ([lng (in-range -180 180 20)])
                          (meridian lng))
                        (make-renderers world-data)))

          #:x-label  #f
          #:y-label  #f
          #:z-label  #f
          #:angle    angle
          #:altitude altitude))

(draw-globe)
