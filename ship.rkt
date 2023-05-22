#lang racket

;; 

(require threading)
(require "api.rkt")

(provide list-my-ships
         ship-status
         ship-location
         ship-inventory-units
         ship-inventory
         list-ship-inventory
         sell-ship-inventory
         inventory-amount
         navigate-ship
         wait-while-in-transit
         ship-cargo-capacity
         dock-ship
         ship-current-fuel
         refuel-ship
         orbit-ship
         extract-ship
         extract-result-cooldown-seconds
         extract-result-capacity
         extract-result-units)

(define (list-my-ships)
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

(define (ship-current-fuel ship-symbol)
  (~> (get-ship-details ship-symbol)
      (hash-ref 'fuel)
      (hash-ref 'current)))

(define (navigate-ship ship-symbol waypoint-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/navigate") "")]
        [data (hash 'waypointSymbol waypoint-symbol)])
    (api-post uri data)))

(define (wait-while-in-transit ship-symbol)
  (define in-transit-status "IN_TRANSIT")
  (define sleep-seconds 10)
  (let status-loop ([current-status (ship-status ship-symbol)])
    (printf "current status ~a~n" current-status)
    (cond
      [(equal? current-status in-transit-status)
       (printf "sleeping ~a seconds~n" sleep-seconds)
       (sleep sleep-seconds)
       (status-loop (ship-status ship-symbol))]
      [else #t]))) 

(define (dock-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/dock") "")])
    (api-post uri #f)))

(define (refuel-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/refuel") "")])
    (api-post uri #f)))

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
    (api-post uri #f)))

(define (extract-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/extract") "")])
    (hash-ref (api-post uri #f 201) 'data)))

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
         
(define (sell-ship-inventory-item ship-symbol trade-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/sell") "")]
        [data (hash 'symbol trade-symbol 'units units)])
    (hash-ref (api-post uri data 201) 'data)))

;; pairs of (symbol . units)
(define (sell-ship-inventory ship-symbol exclude-symbol)
  (for/list ([pair (list-ship-inventory ship-symbol)]
             #:unless (equal? (car pair) exclude-symbol))
    (sell-ship-inventory-item ship-symbol (car pair) (cdr pair))))

