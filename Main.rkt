#lang racket

;; 

(require threading)
(require racket/date)
(require "timestamp.rkt")
(require "api/agents.rkt")
(require "api/contracts.rkt")
(require "api/fleet.rkt")
(require "api/systems.rkt")

(define (inventory-amount trade-symbol ship-inventory)
  (let ([xs (filter (λ (x) (equal? (car x) trade-symbol)) ship-inventory)])
    (cond
      [(null? xs) 0]
      [(equal? (length xs) 1) (cdr (first xs))]
      [else (error (format "invalid inventory ~s;" ship-inventory))])))

(define (process-contract ship-symbol contract-id)
  (for ([deliverable (list-contract-deliverables contract-id)])   
    (let ([delivery-waypoint-symbol (hash-ref deliverable 'destinationSymbol)]
          [trade-symbol (hash-ref deliverable 'tradeSymbol)])
      (let deliverable-loop ([units-fulfilled (hash-ref deliverable 'unitsFulfilled)]
                             [units-required (hash-ref deliverable 'unitsRequired)])
        (cond
          [(equal? units-fulfilled units-required)
           (printf "fulfilling contract ~s~n" contract-id)
           (fulfill-contract contract-id)]
          [else
           (extract-contract-deliverable ship-symbol trade-symbol)
           (let* ([deliverable (process-contract-deliverable ship-symbol
                                                             contract-id
                                                             delivery-waypoint-symbol
                                                             trade-symbol)]
                  [units-fulfilled (hash-ref deliverable 'unitsFulfilled)]
                  [units-required (hash-ref deliverable 'unitsRequired)])
             (deliverable-loop units-fulfilled units-required))])))))
           
(define (process-contract-deliverable ship-symbol contract-id delivery-waypoint-symbol trade-symbol)
  (let ([starting-waypoint-symbol (ship-location ship-symbol)]
        [units (ship-inventory-units (ship-inventory ship-symbol) trade-symbol)])
    
    (printf "navigate: ~a; from ~a to ~a~n"
            ship-symbol starting-waypoint-symbol delivery-waypoint-symbol)
    (let* ([nav-result (navigate-ship ship-symbol delivery-waypoint-symbol)]
           [arrival (nav-result-arrival nav-result)])
      (printf "IN_TRANSIT until ~s~n" (date->string arrival #t))
      (wait (date->seconds arrival #f)))
            
    (dock-ship ship-symbol)
    (refuel-ship ship-symbol)
    
    (printf "delivering ~s units of ~s; ~n" units trade-symbol)
    (let* ([delivery-result (deliver-contract contract-id ship-symbol trade-symbol units)]
           [contract (~>
                      delivery-result
                      (hash-ref 'contract))]
           [deliverable (contract-deliverable contract trade-symbol)]) 
      (printf "deliverable ~s~n" deliverable)

      (orbit-ship ship-symbol)
    
      (printf "navigate: ~a; from ~a to ~a~n"
              ship-symbol delivery-waypoint-symbol starting-waypoint-symbol )
      (let* ([nav-result (navigate-ship ship-symbol starting-waypoint-symbol)]
             [arrival (nav-result-arrival nav-result)])
        (printf "IN_TRANSIT until ~s~n" (date->string arrival #t))
        (wait (date->seconds arrival #f)))
      
      deliverable)))

;; return a survey, or #f if the ship is not equipped for survey
(define (survey-waypoint-if-capable ship-symbol)
  (cond
    [(ship-has-survery-mount? ship-symbol)
     (printf "surveying~n")
     (let* ([survey-result (create-survey ship-symbol)]
            [expiration (cooldown-expiration survey-result)]
            [surveys (hash-ref survey-result 'surveys)])
       (printf "cooling after survey until ~a~n" (date->string expiration #t))
       (wait (date->seconds expiration #f))
       (first surveys))]
    [else #f]))

;; return a hash of (symbol . #t) containing all trade goods at the local market
(define (get-market-trade-goods ship-symbol)
  (let ([trade-goods (list-waypoint-market-trade-goods  (ship-location ship-symbol))])
    (for/hash ([trade-good-item trade-goods])
      (values (hash-ref trade-good-item 'symbol) #t))))
  
;; repeat cycle until we have a full cargo of the deliverable
(define (extract-contract-deliverable ship-symbol contract-goods-symbol)
  (define capacity (ship-cargo-capacity (get-ship ship-symbol)))
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
  (printf "delay 10 sec to avoid HTTP error~n")
  (sleep 10)
  (printf "~a~n" "refueling")
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  (orbit-ship ship-symbol)

  (let ([survey #f ]; (survey-waypoint-if-capable ship-symbol)]
        [market-trade-goods (get-market-trade-goods ship-symbol)]
        [ship-details (get-ship ship-symbol)])

    (when (> (ship-cargo-capacity ship-details) (ship-cargo-units ship-details))
      (printf "~a~n" "extracting")
      (let extract-loop ([extract-result (extract-resources ship-symbol survey)])
        (let* ([expiration (cooldown-expiration extract-result)]
               [capacity (extract-result-capacity extract-result)]
               [units (extract-result-units extract-result)]
               [remaining-capacity (- capacity units)])
          (printf "cooling after extracting until ~a~n" (date->string expiration #t))
          (wait (date->seconds expiration #f))
          (printf "remaining capacity ~a~n" remaining-capacity)
          (cond
            [(zero? remaining-capacity) #t]
            [else (extract-loop (extract-resources ship-symbol survey))]))))
    
    (printf "inventory after extract ~a~n" (list-ship-inventory ship-symbol))
    (printf "~a~n" "docking")
    (dock-ship ship-symbol)

    ;; pairs of (symbol . units)
    (for ([pair (list-ship-inventory ship-symbol)])
      (let ([symbol (car pair)]
            [units (cdr pair)])
        (cond
          [(equal? symbol contract-goods-symbol)
           (printf "retaining ~s units of ~a~n" units symbol)
           #f]
          [(hash-has-key? market-trade-goods symbol)
           (printf "selling ~s units of ~a~n" units symbol)
           (sell-cargo ship-symbol symbol units)
           #t]
          [else 
           (printf "jettisoning ~s units of ~a~n" units symbol)
           (jettison-cargo ship-symbol symbol units)
           #f])))       
    
    (let ([ship-inventory (list-ship-inventory ship-symbol)])
      (printf "inventory after sale ~a~n" ship-inventory)
      (inventory-amount contract-goods-symbol ship-inventory))))