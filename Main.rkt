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

;; generic HTTP POST of URI and JSON data
(define (post uri data access-token [expected-status 200])
  (let-values ([(status-line header-list data-port)
                (http-sendrecv
                 "api.spacetraders.io"
                 uri
                 #:ssl? #t
                 #:method #"POST"
                 #:headers (list (create-auth-header access-token)
                                 "Content-Type: application/json")
                 #:data data)])
    (let-values ([(status reason) (parse-status-line status-line)])
      (unless (= expected-status status)
        (error (format "invalid HTTP status ~s; ~s" status reason))))
    (read-json data-port)))

;; extract system id from (current) waypoint-id
(define (extract-system-id waypoint-id)
  (string-join (take (string-split waypoint-id "-") 2) "-"))

(define (get-agent-details access-token)
  (hash-ref (get "/v2/my/agent" access-token) 'data))

(define (list-waypoints system-id access-token)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints") "")])
    (hash-ref (get uri access-token) 'data)))

(define (get-waypoint-details waypoint-id access-token)  
  (let* (
         [system-id (extract-system-id waypoint-id)]
         [uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id) "")])
    (hash-ref (get uri access-token) 'data)))

(define (waypoint-has-trait? waypoint-details trait-symbol)
  (memf (λ (t) (equal? (hash-ref t 'symbol) trait-symbol)) (hash-ref waypoint-details 'traits)))

(define (list-contracts access-token)
  (hash-ref (get "/v2/my/contracts" access-token) 'data))

(define (accept-contract contract-id access-token)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/accept") "")])
    (post uri #f access-token)))

(define (get-contract-details contract-id access-token)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id) "")])
    (hash-ref (get uri access-token) 'data)))

(define (list-shipyards system-id access-token)
  (filter (λ (wp) (waypoint-has-trait? wp "SHIPYARD")) (list-waypoints system-id access-token)))

(define (list-shipyard-ships system-id shipyard-id access-token)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints/" shipyard-id "/shipyard") "")])
    (hash-ref (get uri access-token) 'data)))
  
(define (purchase-ship ship-type shipyard-waypoint-symbol access-token)
  (let ([uri "/v2/my/ships"]
        [data (hash 'shipType ship-type 'waypointSymbol shipyard-waypoint-symbol)])
    (post uri (jsexpr->string data) access-token 201)))

(define (list-my-ships access-token)
  (hash-ref (get "/v2/my/ships" access-token) 'data))