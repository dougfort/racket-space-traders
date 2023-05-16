#lang racket

;; 

(require net/http-client)
(require json)

;; looking for '("HTTP/1.1" "200" "OK")
;; returning (status reason)
(define (parse-status-line status-line)
  (let ([parts ((compose1 string-split bytes->string/utf-8) status-line)])
    (values ((compose string->number second) parts) (string-join (drop parts 2)))))

;; Authorization: Bearer <access-token>
(define (create-auth-header access-token)
  (string-append "Authorization: "
                 (string-append "Bearer " access-token)))

;; generic HTTP GET of URI
(define (get uri access-token)
  (let-values ([(status-line header-list data-port)
                (http-sendrecv
                 "api.spacetraders.io"
                 uri
                 #:ssl? #t
                 #:method #"GET"
                 #:headers (list (create-auth-header access-token)))])
    (let-values ([(status reason) (parse-status-line status-line)])
      (unless (= 200 status)
        (error (format "invalid HTTP status ~s; ~s" status reason))))
    (read-json data-port)))

(define (get-agent-details access-token)
  (hash-ref (get "/v2/my/agent" access-token) 'data))

(define (get-waypoint-details waypoint-id access-token)
  (define system-id (string-join (take (string-split waypoint-id "-") 2) "-"))
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id) "")])
    (hash-ref (get uri access-token) 'data)))