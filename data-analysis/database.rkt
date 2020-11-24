#lang racket
(require db
         "traffic.rkt")
(provide (all-defined-out))

;; sudo -u postgres psql
;; postgres=# create database traffic;
;; postgres=# create user traffic_user with encrypted password ' ';
;; postgres=# grant all privileges on database traffic to traffic_user;

;; Currently does not work...
(define (setup-postgres-db postgres-password)
  (define admin-pgc (postgresql-connect #:user     "postgres"
                                        #:password postgres-password))
  (query-exec admin-pgc "create database traffic")
  (query-exec admin-pgc "create user traffic_user with encrypted password ' '")
  (query-exec admin-pgc "grant all privileges on database traffic to traffic_user")
  (disconnect admin-pgc))

(define pgc
  (with-handlers ([exn:fail:sql? (lambda (e) (setup-postgres-db " "))])
    (postgresql-connect #:user     "traffic_user"
                        #:database "traffic"
                        #:password " ")))

(define (insert-data x)
  (unless (query-maybe-value pgc
                             "select timestamp from traffic_data where report_id = $1 and timestamp = $2 fetch first row only"
                             (report-id x)
                             (string->sql-timestamp (timestamp x)))
    (query-exec pgc
                "insert into traffic_data values ($1, $2, $3, $4, $5, $6, $7, $8)"
                (-id x)
                (report-id x)
                (string->sql-timestamp (timestamp x))
                (status x)
                (average-measured-time x)
                (median-measured-time x)
                (vehicle-count x)
                (average-speed x))))

(define (insert-metadata x)
  (unless (query-maybe-value pgc
                             "select report_id from traffic_metadata where report_id = $1 fetch first row only"
                             (report-id x))
    (define query-string (string-append
                          "insert into traffic_metadata values ("
                          (string-join
                           (for/list ([i (in-range 1 26)])
                             (format "$~a" i))
                           ", ")
                          ")"))
    (query-exec pgc
                query-string
                (-id x)
                (report-name x)
                (report-id x)
                (organisation x)
                (rba-id x)
                (point-1-name x)
                (point-1-latitude x)
                (point-1-longitude x)
                (point-1-street x)
                (point-1-street-number x)
                (point-1-postal-code x)
                (point-1-country x)
                (point-2-name x)
                (point-2-latitude x)
                (point-2-longitude x)
                (point-2-street x)
                (point-2-street-number x)
                (point-2-postal-code x)
                (point-2-country x)
                (duration-in-seconds x)
                (distance-in-meters x)
                (ndt-in-kmh x)
                (road-type x)
                (point-1-city x)
                (point-2-street x))))

(define (string->sql-timestamp t)
  (match-define (list da ti)                      (string-split t  "T"))
  (match-define (list y mo d) (map string->number (string-split da "-")))
  (match-define (list h m s)  (map string->number (string-split ti ":")))
  (sql-timestamp y mo d h m s 0 0))

(define (sql-timestamp->string s)
  (match-define (sql-timestamp y mo d h m _ _ _) s)
  (format "~a:~a ~a-~a-~a"
          (~r h  #:min-width 2 #:pad-string "0")
          (~r m  #:min-width 2 #:pad-string "0")
          (~r d  #:min-width 2 #:pad-string "0")
          (~r mo #:min-width 2 #:pad-string "0")
          (~r y  #:min-width 2 #:pad-string "0")))

(define (get-all-report-ids [db pgc])
  (query-list db
              "select report_id from traffic_metadata order by report_id"))

(define (lookup-from-id value report-id)
  (query-maybe-value pgc
                     (format "select ~a from traffic_metadata where report_id = $1"
                             value)
                     report-id))

(define (auto-fill-data-from [start data-start])
  (define-values (d n) (get-records-from start))
  (unless (empty? d)
    (for-each insert-data d)
    (auto-fill-data-from n)))

(define (auto-fill-metadata-from [start metadata-start])
  (define-values (d n) (get-records-from start))
  (unless (empty? d)
    (for-each insert-metadata d)
    (auto-fill-metadata-from n)))

(define (reset-tables db)
  (query-exec db "drop view full_traffic")
  (query-exec db "drop table traffic_data")
  (query-exec db "drop table traffic_metadata")
  (create-data-table db)
  (create-metadata-table db)
  (create-full-view db))

(define (create-data-table db)
  (query-exec db "create table traffic_data (
id                   integer,
report_id            integer,
timestamp            timestamp,
status               varchar(50),
avg_measured_time    integer,
median_measured_time integer,
vehicle_count        integer,
avg_speed            integer
)"))

(define (create-metadata-table db)
  (query-exec db "create table traffic_metadata (
id                    integer,
report_name           varchar(50),
report_id             integer,
organisation          varchar(50),
rba_id                integer,
point_1_name          varchar(50),
point_1_lat           float,
point_1_lng           float,
point_1_street        varchar(50),
point_1_street_number varchar(50),
point_1_postal_code   varchar(50),
point_1_country       varchar(50),
point_2_name          varchar(50),
point_2_lat           float,
point_2_lng           float,
point_2_street_number varchar(50),
point_2_postal_code   varchar(50),
point_2_city          varchar(50),
point_2_country       varchar(50),
duration_in_sec       integer,
distance_in_meters    integer,
ndt_in_kmh            integer,
road_type             varchar(50),
point_1_city          varchar(50),
point_2_street        varchar(50)
)"))

(define (create-full-view db)
  (query-exec db "create or replace view full_traffic as (
select
data.timestamp,
data.status,
data.avg_measured_time,
data.median_measured_time,
data.vehicle_count,
data.avg_speed,
data.avg_speed > meta.ndt_in_kmh as speeding,
meta.*
from traffic_data data
join traffic_metadata meta
on data.report_id = meta.report_id
)"))

(define (auto-update)
  (let loop ()
    (auto-fill-data-from)
    (auto-fill-metadata-from)
    (sleep 100)
    (loop)))

(define (run-db)
  (thread auto-update))
