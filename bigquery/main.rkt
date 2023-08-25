#lang racket
(require json
         net/url
         net/base64
         net/jwt)

(define base-bigquery-url (string->url "https://bigquery.googleapis.com"))

(define serviceaccount-auth-data (string->jsexpr (getenv "GOOGLE_SERVICE_ACCOUNT_DATA")))

(define google-oauth-url (string->url "https://accounts.google.com/o/oauth2/v2/auth"))

#;(define (jwt-claim sa-data)
    (encode/sign "RS256" ""
                 #:extra-headers (hasheq 'typ "JWT" 'kid "")
                 #:iss (hash-ref sa-data 'client_email)
                 #:aud (hash-ref sa-data 'token_uri)
                 #:iat (current-seconds)
                 #:exp (+ (current-seconds) 86400)))

(define jwt-auth
  (encode/sign "HS256" (getenv "GOOGLE_JWT_SECRET")
               #:extra-headers (hasheq 'typ "JWT")
               #:iat (current-seconds)
               #:exp (+ (current-seconds) (* 24 60 60))))

(define issuu-cookie (format "Cookie: issuu.serviceclient.token=~A" jwt-auth))

(define content-type "Content-Type: application/json")

(define headers (list content-type issuu-cookie))

(define doc-url (string->url "https://serviceclient.issuu.com/endpoint/call/entity-document/GetDocument?routingKey=entity-document.v1.get&exchange=amq.direct&timeout=3000"))

(port->string (post-pure-port doc-url
                              (jsexpr->bytes (hasheq 'documentSlotId 14466))
                              headers))

#;(define (build-google-oauth-query google-url auth-token)
    (combine-url/relative google-url
                          (format "?client_id=~A&response_type=code&" (hash-ref auth-token 'client_id))
                          ))


#;(call/input-url google-oauth-url
                  (lambda (u) (get-pure-port u #:redirections 1))
                  (compose #;string->jsexpr port->string))
