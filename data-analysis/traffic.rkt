#lang racket
(require (for-syntax syntax/parse)
         net/url
         json)
(provide (all-defined-out))

;; Real-time traffic data
(define base-url       "https://admin.opendata.dk")
(define metadata-start
  "/api/3/action/datastore_search?resource_id=c3097987-c394-4092-ad1d-ad86a81dbf37")
(define data-start
  "/api/3/action/datastore_search?resource_id=b3eeb0ff-c8a8-4824-99d6-e0a3747c8b0d")

;; string -> url
(define (build-url str)
  (string->url (string-append base-url str)))

;; url -> json
(define retrieve-json
  (compose read-json get-pure-port))

(define (get-records-from start [count 20])
  (let ([stop? #f])
    (for/fold ([data empty]
               [next start])
              ([i    (in-range count)]
               #:break stop?)
      (define js      (retrieve-json (build-url next)))
      (define records (hash-ref (hash-ref js 'result) 'records))
      (set! stop? (empty? records))
      (values (append records data)
              (hash-ref (hash-ref (hash-ref js 'result) '_links) 'next)))))

(define-syntax (define/handle-null stx)
  (define type-defaults (hasheq 'string    ""
                                'int       +nan.0
                                'float     +nan.f
                                'timestamp ""))
  (syntax-parse stx
    #:literals (quote)
    [(_ (define-id:id ...) (quote type:id) body:expr ...)
     (cond
       [(member (syntax->datum #'type) (hash-keys type-defaults))
        #`(define (define-id ...)
            (let ([r (begin body ...)])
              (if (symbol? r)
                  #,(hash-ref type-defaults (syntax->datum #'type))
                  r)))]
       [else
        (raise-syntax-error #f
                            (format "Expected argument from '~a"
                                    (hash-keys type-defaults))
                            stx
                            #'type)])]
    [(_ define-id type body ...)
     #'(define/handle-null define-id (quote type)
         body ...)]))

(define/handle-null (-id x) 'int
  (hash-ref x '_id))
(define/handle-null (report-id x) 'int
  (hash-ref x 'REPORT_ID))
(define/handle-null (timestamp x) 'timestamp
  (hash-ref x 'TIMESTAMP))
(define/handle-null (point-1-street-number x) 'string
  (hash-ref x 'POINT_1_STREET_NUMBER))
(define/handle-null (point-2-street-number x) 'string
  (hash-ref x 'POINT_2_STREET_NUMBER))
(define/handle-null (point-1-postal-code x) 'string
  (hash-ref x 'POINT_1_POSTAL_CODE))
(define/handle-null (point-2-postal-code x) 'string
  (hash-ref x 'POINT_2_POSTAL_CODE))
(define/handle-null (point-1-city x) 'string
  (hash-ref x 'POINT_1_CITY))
(define/handle-null (point-2-city x) 'string
  (hash-ref x 'POINT_2_CITY))
(define/handle-null (point-1-street x) 'string
  (hash-ref x 'POINT_1_STREET))
(define/handle-null (point-2-street x) 'string
  (hash-ref x 'POINT_2_STREET))
(define/handle-null (average-measured-time x) 'int
  (hash-ref x 'avgMeasuredTime))
(define/handle-null (average-speed x) 'int
  (hash-ref x 'avgSpeed))
(define/handle-null (median-measured-time x) 'int
  (hash-ref x 'medianMeasuredTime))
(define/handle-null (status x) 'string
  (hash-ref x 'status))
(define/handle-null (vehicle-count x) 'int
  (hash-ref x 'vehicleCount))
(define/handle-null (report-name x) 'string
  (hash-ref x 'REPORT_NAME))
(define/handle-null (organisation x) 'string
  (hash-ref x 'ORGANISATION))
(define/handle-null (rba-id x) 'int
  (hash-ref x 'RBA_ID))
(define/handle-null (point-1-name x) 'string
  (hash-ref x 'POINT_1_NAME))
(define/handle-null (point-1-latitude x) 'float
  (hash-ref x 'POINT_1_LAT))
(define/handle-null (point-1-longitude x) 'float
  (hash-ref x 'POINT_1_LNG))
(define/handle-null (point-1-country x) 'string
  (hash-ref x 'POINT_1_COUNTRY))
(define/handle-null (point-2-name x) 'string
  (hash-ref x 'POINT_2_NAME))
(define/handle-null (point-2-latitude x) 'float
  (hash-ref x 'POINT_2_LAT))
(define/handle-null (point-2-longitude x) 'float
  (hash-ref x 'POINT_2_LNG))
(define/handle-null (point-2-country x) 'string
  (hash-ref x 'POINT_2_COUNTRY))
(define/handle-null (duration-in-seconds x) 'int
  (hash-ref x 'DURATION_IN_SEC))
(define/handle-null (distance-in-meters x) 'int
  (hash-ref x 'DISTANCE_IN_METERS))
(define/handle-null (ndt-in-kmh x) 'int
  (hash-ref x 'NDT_IN_KMH))
(define/handle-null (road-type x) 'string
  (hash-ref x 'ROAD_TYPE))
