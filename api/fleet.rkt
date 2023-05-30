#lang racket

;; 

(require threading)
(require "http.rkt")
(require "timestamp.rkt")

(provide list-ships
         ship-status
         ship-location
         ship-inventory-units
         ship-inventory
         list-ship-inventory
         sell-ship-inventory-item
         jettison-ship-inventory-item
         inventory-amount
         navigate-ship
         nav-result-arrival
         ship-cargo-capacity
         ship-cargo-units
         dock-ship
         ship-current-fuel
         refuel-ship
         orbit-ship
         ship-has-survery-mount?
         survey-waypoint
         extract-ship
         cooldown-expiration
         extract-result-capacity
         extract-result-units)

(define (list-ships)
  (hash-ref (api-get "/v2/my/ships") 'data))

(define (get-ship-details ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol) "")])
    (hash-ref (api-get uri) 'data)))

(define (ship-status ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'nav)
      (hash-ref 'status)))

(define (ship-location ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'nav)
      (hash-ref 'waypointSymbol)))

(define (ship-inventory ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'inventory)))

(define (ship-inventory-units inventory trade-symbol)
  (for/or ([item inventory])
    (cond
      [(equal? (hash-ref item 'symbol) trade-symbol) (hash-ref item 'units)]
      [else #f])))

(define (ship-cargo-capacity ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'capacity)))

(define (ship-cargo-units ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'units)))

(define (ship-current-fuel ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'fuel)
      (hash-ref 'current)))

(define (navigate-ship ship-symbol waypoint-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/navigate") "")]
        [data (hash 'waypointSymbol waypoint-symbol)])
    (hash-ref (api-post uri data) 'data)))

(define (nav-result-arrival nav-result)
  (parse-timestamp (~> nav-result
                       (hash-ref 'nav)
                       (hash-ref 'route)
                       (hash-ref 'arrival))))

(define (dock-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/dock") "")])
    (hash-ref (api-post uri #f) 'data)))

(define (refuel-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/refuel") "")])
    (hash-ref (api-post uri #f) 'data)))

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
    (hash-ref (api-post uri #f) 'data)))

(define (ship-has-survery-mount? ship-symbol)
  (let ([mounts (~> (get-ship-details ship-symbol)
                    (hash-ref 'mounts))])
    (findf (λ (m) (string-prefix? (hash-ref m 'symbol) "MOUNT_SURVEYOR")) mounts)))                

(define (survey-waypoint ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/survey") "")])
    (hash-ref (api-post uri #f 201) 'data)))

(define (extract-ship ship-symbol survey)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/extract") "")]
        [data (if survey (hash 'survey survey) #f)])
    (hash-ref (api-post uri data 201) 'data)))

(define (cooldown-expiration result)
  (parse-timestamp (~> result
                       (hash-ref 'cooldown)
                       (hash-ref 'expiration))))  
         
(define (extract-result-capacity extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'capacity)))
         
(define (extract-result-units extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'units)))
         
(define (sell-ship-inventory-item ship-symbol trade-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/sell") "")]
        [data (hash 'symbol trade-symbol 'units units)])
    (hash-ref (api-post uri data 201) 'data)))

(define (jettison-ship-inventory-item ship-symbol trade-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/jettison") "")]
        [data (hash 'symbol trade-symbol 'units units)])
    (hash-ref (api-post uri data 200) 'data)))

(define (ship-negotiate-contract ship-symbol )
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/negotiate/contract") "")])
    (hash-ref (api-post uri #f 201) 'data)))

