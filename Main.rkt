#lang racket

;; 

(require net/http-client)
(require json)
(require threading)
(require "access.rkt")

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
(define (get uri)
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
(define (post uri data [expected-status 200])
  (let-values ([(status-line header-list data-port)
                (http-sendrecv
                 "api.spacetraders.io"
                 uri
                 #:ssl? #t
                 #:method #"POST"
                 #:headers (list (create-auth-header)
                                 "Content-Type: application/json")
                 #:data data)])
    (let-values ([(status reason) (parse-status-line status-line)])
      (unless (= expected-status status)
        (error (format "invalid HTTP status ~s; ~s; ~s" status reason uri))))
    (read-json data-port)))

;; extract system id from (current) waypoint-id
(define (extract-system-id waypoint-id)
  (string-join (take (string-split waypoint-id "-") 2) "-"))

(define (get-agent-details)
  (hash-ref (get "/v2/my/agent") 'data))

(define (agent-headquarters)
  (~> (get-agent-details)
      (hash-ref 'headquarters)))

(define (list-waypoints system-id)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints") "")])
    (hash-ref (get uri) 'data)))

(define (get-waypoint-details waypoint-id)  
  (let* (
         [system-id (extract-system-id waypoint-id)]
         [uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id) "")])
    (hash-ref (get uri) 'data)))

(define (waypoint-has-trait? waypoint-details trait-symbol)
  (memf (λ (t) (equal? (hash-ref t 'symbol) trait-symbol)) (hash-ref waypoint-details 'traits)))

(define (waypoint-type waypoint-details)
  (hash-ref waypoint-details 'type))

(define (list-contracts)
  (hash-ref (get "/v2/my/contracts") 'data))

(define (accept-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/accept") "")])
    (post uri #f)))

(define (get-contract-details contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id) "")])
    (hash-ref (get uri) 'data)))

(define (list-waypoints-with-shipyard system-id)
  (filter (λ (wp) (waypoint-has-trait? wp "SHIPYARD")) (list-waypoints system-id)))

(define (list-asteroid-field-waypoints system-id)
  (filter (λ (wp) (equal? (waypoint-type wp) "ASTEROID_FIELD")) (list-waypoints system-id)))

(define (list-shipyard-ships system-id shipyard-id)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints/" shipyard-id "/shipyard") "")])
    (hash-ref (get uri) 'data)))
  
(define (purchase-ship ship-type shipyard-waypoint-symbol)
  (let ([uri "/v2/my/ships"]
        [data (hash 'shipType ship-type 'waypointSymbol shipyard-waypoint-symbol)])
    (post uri (jsexpr->string data) 201)))

(define (list-my-ships)
  (hash-ref (get "/v2/my/ships") 'data))

(define (get-ship-details ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol) "")])
    (hash-ref (get uri) 'data)))

(define (ship-status ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'nav)
      (hash-ref 'status)))

(define (ship-inventory ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'inventory)))

(define (ship-cargo-capacity ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'capacity)))

(define (ship-current-fuel ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'fuel)
      (hash-ref 'current)))

(define (navigate-ship ship-symbol waypoint-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/navigate") "")]
        [data (hash 'waypointSymbol waypoint-symbol)])
    (post uri (jsexpr->string data))))

(define (dock-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/dock") "")])
    (post uri #f)))

(define (refuel-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/refuel") "")])
    (post uri #f)))

(define (list-ship-inventory ship-symbol)
  (for/list ([item (ship-inventory ship-symbol)])
    (cons (hash-ref item 'symbol) (hash-ref item 'units))))

(define (inventory-amount trade-symbol ship-inventory)
  (let ([xs (filter (λ (x) (equal? (car x) trade-symbol)) ship-inventory)])
    (cond
      [(null? xs) 0]
      [(equal? (length xs) 1) (cdr (first xs))]
      [else (error (format "invalid inventory ~s;" ship-inventory))])))
  
(define (orbit-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/orbit") "")])
    (post uri #f)))

(define (extract-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/extract") "")])
    (hash-ref (post uri #f 201) 'data)))

(define (extract-result-cooldown-seconds extract-result)
  (~> extract-result
      (hash-ref 'cooldown)
      (hash-ref 'remainingSeconds)))  
         
(define (extract-result-capacity extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'capacity)))
         
(define (extract-result-units extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'units)))
         
(define (list-waypoint-market-data waypoint-id)
  (let* (
         [system-id (extract-system-id waypoint-id)]
         [uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id "/market") "")])
    (hash-ref (get uri) 'data)))

(define (sell-ship-inventory-item ship-symbol trade-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/sell") "")]
        [data (hash 'symbol trade-symbol 'units units)])
    (hash-ref (post uri (jsexpr->string data) 201) 'data)))

;; pairs of (symbol . units)
(define (sell-ship-inventory ship-symbol exclude-symbol)
  (for/list ([pair (list-ship-inventory ship-symbol)]
        #:unless (equal? (car pair) exclude-symbol))
    (sell-ship-inventory-item ship-symbol (car pair) (cdr pair))))

;; repeat cycle until we have enough to fulfill our contract
(define (extract-contract-cargo ship-symbol contract-goods-symbol)
  (define capacity (ship-cargo-capacity ship-symbol))
  (let contract-loop ([units (extract-and-sell-cargo ship-symbol contract-goods-symbol)])
    (printf "contract cargo units: ~a~n" units)
    (cond
      [(equal? units capacity) #t]
      [else (contract-loop (extract-and-sell-cargo ship-symbol contract-goods-symbol))])))

;;   - dock
;;   - refuel
;;   - orbit
;;   - extract until full
;;   - dock
;;   - sell cargo
;; return the amount of contract cargo in inventory
(define (extract-and-sell-cargo ship-symbol contract-goods-symbol)
  (printf "~a~n" "docking")
  (dock-ship ship-symbol)
  (printf "refueling: started with ~a~n" (ship-current-fuel ship-symbol))
  (refuel-ship ship-symbol)
  (printf "orbiting: fuel ~a~n" (ship-current-fuel ship-symbol))
  (orbit-ship ship-symbol)
  (printf "~a~n" "extracting")
  (let extract-loop ([extract-result (extract-ship ship-symbol)])
    (let* ([seconds-remaining (extract-result-cooldown-seconds extract-result)]
           [capacity (extract-result-capacity extract-result)]
           [units (extract-result-units extract-result)]
           [remaining-capacity (- capacity units)])
      (printf "cooling ~a seconds~n" seconds-remaining)
      (sleep seconds-remaining)
      (printf "remaining capacity ~a~n" remaining-capacity)
      (cond
        [(zero? remaining-capacity) #t]
        [else (extract-loop (extract-ship ship-symbol))])))                                    
  (printf "inventory after extract ~a~n" (list-ship-inventory ship-symbol))
  (printf "~a~n" "docking")
  (dock-ship ship-symbol)
  (printf "selling. retaining ~a~n" contract-goods-symbol)
  (sell-ship-inventory ship-symbol contract-goods-symbol)
  (let ([ship-inventory (list-ship-inventory ship-symbol)])
    (printf "inventory after sale ~a~n" ship-inventory)
    (inventory-amount contract-goods-symbol ship-inventory)))