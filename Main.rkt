#lang racket

;; 

(require threading)
(require "api.rkt")
(require "agent.rkt")
(require "contract.rkt")
(require "ship.rkt")
(require "waypoint.rkt")

(define (agent-system-id)
  (extract-system-id (agent-headquarters)))

(define (locate-asteroid-field system-id)
  (hash-ref (first (list-asteroid-field-waypoints system-id)) 'symbol))

(define (process-contract ship-symbol contract-id)
  (for ([deliverable (list-contract-deliverables contract-id)])   
    (let ([delivery-waypoint-symbol (hash-ref deliverable 'destinationSymbol)]
          [trade-symbol (hash-ref deliverable 'tradeSymbol)])
      (let deliverable-loop ([units-fulfilled (hash-ref deliverable 'unitsFulfilled)]
                             [units-required (hash-ref deliverable 'unitsRequired)])
        (cond
          [(equal? units-fulfilled units-required) #t]
          [else
           (extract-contract-deliverable ship-symbol trade-symbol)
           (let* ([deliverable (process-contract-deliverable ship-symbol
                                                             contract-id
                                                             delivery-waypoint-symbol trade-symbol)]
                  [units-fulfilled (hash-ref deliverable 'unitsFulfilled)]
                  [units-required (hash-ref deliverable 'unitsRequired)])
             (deliverable-loop units-fulfilled units-required))])))))
           
(define (process-contract-deliverable ship-symbol contract-id delivery-waypoint-symbol trade-symbol)
  (let ([starting-waypoint-symbol (ship-location ship-symbol)]
        [units (ship-inventory-units (ship-inventory ship-symbol) trade-symbol)])
    
    (printf "navigate: ~a; from ~a to ~a~n"
            ship-symbol starting-waypoint-symbol delivery-waypoint-symbol)
    (navigate-ship ship-symbol delivery-waypoint-symbol)
    (wait-while-in-transit ship-symbol)
    
    (dock-ship ship-symbol)
    (refuel-ship ship-symbol)
    
    (printf "delivering ~s units of ~s; ~n" units trade-symbol)
    (let* ([delivery-result (contract-deliver-cargo contract-id ship-symbol trade-symbol units)]
           [contract (~>
                      delivery-result
                      (hash-ref 'data)
                      (hash-ref 'contract))]
           [deliverable (contract-deliverable contract trade-symbol)]) 
      (printf "deliverable ~s~n" deliverable)

      (orbit-ship ship-symbol)
    
      (printf "navigate: ~a; from ~a to ~a~n"
              ship-symbol delivery-waypoint-symbol starting-waypoint-symbol )
      (navigate-ship ship-symbol starting-waypoint-symbol)
      (wait-while-in-transit ship-symbol)
      deliverable)))

;; repeat cycle until we have a full cargo of the deliverable
(define (extract-contract-deliverable ship-symbol contract-goods-symbol)
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
  (printf "~a~n" "refueling")
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  (orbit-ship ship-symbol)

  ;; the mining drone doesn't have a survey mount
  (define survey-result #f)

  (printf "~a~n" "extracting")
  (let extract-loop ([extract-result (extract-ship ship-symbol survey-result)])
    (let* ([seconds-remaining (extract-result-cooldown-seconds extract-result)]
           [capacity (extract-result-capacity extract-result)]
           [units (extract-result-units extract-result)]
           [remaining-capacity (- capacity units)])
      (printf "cooling ~a seconds~n" seconds-remaining)
      (sleep seconds-remaining)
      (printf "remaining capacity ~a~n" remaining-capacity)
      (cond
        [(zero? remaining-capacity) #t]
        [else (extract-loop (extract-ship ship-symbol survey-result))])))
    
  (printf "inventory after extract ~a~n" (list-ship-inventory ship-symbol))
  (printf "~a~n" "docking")
  (dock-ship ship-symbol)
  (printf "selling. retaining ~a~n" contract-goods-symbol)
  (sell-ship-inventory ship-symbol contract-goods-symbol)
  (let ([ship-inventory (list-ship-inventory ship-symbol)])
    (printf "inventory after sale ~a~n" ship-inventory)
    (inventory-amount contract-goods-symbol ship-inventory)))