#lang racket
(require plot
         plot/utils)

(provide make-line-renderers
         make-3d-renderers
         make-point-renderers)

(define (make-polygon-renderer polygons renderer)
  (for/fold ([renderers '()])
            ([polygon (in-list polygons)]
             #:unless (null? polygon))
    (define points
      (for/list ([point (in-sequences (in-list polygon)
                                      (in-value (car polygon)))])
        (match-define (list lng lat _ ...) point)
        (list lng lat)))
    (cons (renderer points) renderers)))

(define (make-point-renderer point renderer)
  (match-define (list lng lat _ ...) point)
  (renderer (list (list lng lat))))

(define (make-renderers geojson-data renderer)
  (for/fold ([renderers '()])
            ([feature   (in-list (hash-ref geojson-data 'features))])
    (let* ([geometry (hash-ref feature 'geometry (lambda () (hash)))]
           [data     (hash-ref geometry 'coordinates (lambda () null))])
      (case (hash-ref geometry 'type #f)
        [("Polygon")      (cons (make-polygon-renderer data renderer) renderers)]
        [("MultiPolygon") (cons (for/list ([polygon (in-list data)])
                                  (make-polygon-renderer polygon renderer))
                                renderers)]
        [("Point")        (cons (make-point-renderer data renderer) renderers)]
        [else             (printf "Skipping ~A geometry~%" (hash-ref geometry 'type #f))
                          renderers]))))

(define (make-line-renderers geojson-data)
  (make-renderers geojson-data lines))

;; A point is a list '(lng lat ...)
(define (point->unit-vector p)
  (3d-polar->3d-cartesian (degrees->radians (first p))
                          (degrees->radians (second p))
                          1))

(define (make-3d-renderers geojson-data)
  (make-renderers geojson-data (lambda (points)
                                 (lines3d (map point->unit-vector points)))))

(define (make-point-renderers geojson-data)
  (make-renderers geojson-data points))
