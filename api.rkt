#lang racket

;; 

(require net/http-client)
(require json)
(require "access.rkt")

(provide api-get api-post)

;; looking for '("HTTP/1.1" "200" "OK")
;; returning (status reason)
(define (parse-status-line status-line)
  (let ([parts ((compose1 string-split bytes->string/utf-8) status-line)])
    (values ((compose string->number second) parts) (string-join (drop parts 2)))))

;; Authorization: Bearer <access-token>
(define (create-auth-header)
  (string-append "Authorization: "
                 (string-append "Bearer " access-token)))

;; generic HTTP GET of URI
(define (api-get uri)
  (unless access-token (load-access-token))
  (let-values ([(status-line header-list data-port)
                (http-sendrecv
                 "api.spacetraders.io"
                 uri
                 #:ssl? #t
                 #:method #"GET"
                 #:headers (list (create-auth-header)))])
    (let-values ([(status reason) (parse-status-line status-line)])
      (unless (= 200 status)
        (error (format "invalid HTTP status ~s; ~s" status reason))))
    (read-json data-port)))

;; generic HTTP POST of URI and JSON data
(define (api-post uri data [expected-status 200])
  (unless access-token (load-access-token))
  (let ([post-data (cond
                     [(hash? data) (jsexpr->string data)]
                     [else data])])
    (let-values ([(status-line header-list data-port)
                  (http-sendrecv
                   "api.spacetraders.io"
                   uri
                   #:ssl? #t
                   #:method #"POST"
                   #:headers (list (create-auth-header)
                                   "Content-Type: application/json")
                   #:data post-data)])
      (let-values ([(status reason) (parse-status-line status-line)])
        (unless (= expected-status status)
          (error (format "invalid HTTP status ~s; ~s; ~s" status reason uri))))
      (read-json data-port))))

