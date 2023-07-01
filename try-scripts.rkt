#lang racket

;; 

(require threading)
(require racket/date)
(require space-traders-v2/agents)
(require space-traders-v2/contracts)
(require space-traders-v2/factions)
(require space-traders-v2/fleet)
(require space-traders-v2/systems)
(require space-traders-v2/wrappers)
(require "timestamp.rkt")
(require "lenses/agent.rkt")
(require "lenses/contract.rkt")
(require "lenses/ship.rkt")
(require "lenses/ship-nav.rkt")
(require "lenses/cargo.rkt")
(require "lenses/cooldown.rkt")
(require "lenses/contract-deliver-good.rkt")
(require "lenses/all.rkt")
(require "wait-queue.rkt")
(require "distance.rkt")
(require "directory.rkt")

;; return a hash of (symbol . #t) containing all trade goods at the local market
(define (get-market-trade-goods system-id waypoint-id)    
  (let* ([market-details (get-market system-id waypoint-id)]
         [trade-goods (~> market-details
                          (hash-ref 'data)
                          (hash-ref 'tradeGoods '()))])
    (for/hash ([trade-good-item trade-goods])
      (values (hash-ref trade-good-item 'symbol) #t))))
  
(define (current-utc-date)
  (seconds->date (current-seconds) #f))

(define (increment-current-utc-date amount)
  (seconds->date (+ (current-seconds) amount) #f))

(define check-count
  (λ (script-id state) (let ([timestamp (current-utc-date)]
                             [next-count (add1 (hash-ref state 'count 0))]
                             [max-count (hash-ref state 'max-count 1)])
                         (printf "check-count: next ~s; max ~s~n" next-count max-count)
                         (cond                                
                           [(>= next-count max-count)
                            (task-result timestamp 'increment (hash-set state 'count 0))]
                           [else
                            (task-result timestamp 'repeat (hash-set state 'count next-count))]))))

;; navigate to agent headquarters
;; then navigate to the asteroid field and back
(define (build-navigate-script ship-id asteroid-field)
  (define hq (agent-headquarters (data (get-agent))))
  (define navigate-to-hq
    (λ (script-id state) (let ([timestamp (navigate ship-id hq)])
                           (task-result timestamp 'increment state)))) 
  (define navigate-to-asteroid-field
    (λ (script-id state) (let ([timestamp (navigate ship-id asteroid-field)])
                           (task-result timestamp 'increment state))))     
     
  (list->vector (list
                 (task null navigate-to-hq)
                 (task 'repeat navigate-to-asteroid-field)
                 (task null navigate-to-hq)
                 (task null check-count))))

(define (build-negotiate-script ship-id)
  (define hq (agent-headquarters))
  (define navigate-to-hq
    (λ (script-id state) (let ([timestamp (navigate ship-id hq)])
                           (task-result timestamp 'increment state)))) 
  (define negotiate-at-hq
    (λ (script-id state) (let ([timestamp (current-utc-date)])
                           (negotiate ship-id)
                           (task-result timestamp 'increment state))))      
  (list->vector (list
                 (task null navigate-to-hq)
                 (task null negotiate-at-hq))))

(define (build-extract-loop-script ship-id source-id system-id market-id-1 market-id-2)
  (define navigate-to-source
    (λ (script-id state) (let ([timestamp (navigate ship-id source-id)])
                           (task-result timestamp 'increment state)))) 
  (define navigate-to-market-1
    (λ (script-id state) (let ([timestamp (navigate ship-id market-id-1)])
                           (task-result timestamp 'increment state)))) 
  
  (define navigate-to-market-2
    (λ (script-id state) (let ([timestamp (navigate ship-id market-id-2)])
                           (task-result timestamp 'increment state)))) 
  
  (define extract-from-current-location
    (λ (script-id state) (let-values ([(timestamp remaining-capacity) (extract ship-id)])
                           (printf "extract-from-current-location: remaining capacity ~s~n" remaining-capacity)
                           (cond
                             [(zero? remaining-capacity) (task-result timestamp 'increment state)]
                             [else (task-result timestamp 'extract state)])))) 
  
  (define sell-cargo-at-market-1
    (λ (script-id state) (let ([timestamp (sell ship-id system-id market-id-1)])
                           (task-result timestamp 'increment state)))) 
  
  (define sell-cargo-at-market-2
    (λ (script-id state) (let ([timestamp (sell ship-id system-id market-id-2)])
                           (task-result timestamp 'increment state)))) 
  
  (define jettison-all-cargo
    (λ (script-id state) (let ([timestamp (jettison ship-id)])
                           (task-result timestamp 'increment state)))) 
  
  (list->vector (list
                 (task 'repeat navigate-to-source)
                 (task null jettison-all-cargo)
                 (task 'extract extract-from-current-location)
                 (task null navigate-to-market-1)
                 (task null sell-cargo-at-market-1)
                 (task null navigate-to-market-2)
                 (task null sell-cargo-at-market-2)
                 (task null check-count))))

(define (build-contract-loop-script contract-id
                                    contract-cargo-id
                                    ship-id source-id
                                    system-id
                                    contract-dest-id
                                    market-dest-id)
  (define navigate-to-source
    (λ (script-id state) (let ([timestamp (navigate ship-id source-id)])
                           (task-result timestamp 'increment state))))
  
  (define navigate-to-contract-dest
    (λ (script-id state) (let ([timestamp (navigate ship-id contract-dest-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define navigate-to-market-dest
    (λ (script-id state) (let ([timestamp (navigate ship-id market-dest-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define survey-local-waypoint
    (λ (script-id state) (let-values ([(timestamp next-state) (survey state ship-id)])
                           (task-result timestamp 'increment next-state))))
  
  (define extract-from-current-location
    (λ (script-id state) (let-values ([(timestamp remaining-capacity) (extract ship-id)])
                           (printf "extract-from-current-location: remaining capacity ~s~n" remaining-capacity)
                           (cond
                             [(zero? remaining-capacity) (task-result timestamp 'increment state)]
                             [else (task-result timestamp 'extract state)])))) 
  
  (define deliver-contract-cargo-at-dest
    (λ (script-id state) (let ([timestamp (deliver contract-id ship-id contract-cargo-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define sell-cargo-at-contract-dest
    (λ (script-id state) (let ([timestamp (sell ship-id system-id contract-dest-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define sell-cargo-at-market-dest
    (λ (script-id state) (let ([timestamp (sell ship-id system-id market-dest-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define jettison-all-cargo
    (λ (script-id state) (let ([timestamp (jettison ship-id)])
                           (task-result timestamp 'increment state))))

  (define check-contract-status
    ;; TODO: handle multiple deliverables
    (λ (script-id state) (let-values ([(timestamp units-needed)
                                       (compute-units-needed contract-id contract-cargo-id)])
                           (cond
                             [(zero? units-needed) (task-result timestamp 'increment state)]
                             [else (task-result timestamp 'repeat state)]))))
                                      
  (define mark-contract-fulfilled
    (λ (script-id state) (let ([timestamp (fulfill contract-id)])
                           (task-result timestamp 'increment state))))

  (list->vector (list
                 (task 'repeat navigate-to-source)
                 (task null jettison-all-cargo)
                 (task 'extract survey-local-waypoint)
                 (task null extract-from-current-location)
                 (task null navigate-to-contract-dest)
                 (task null deliver-contract-cargo-at-dest)
                 (task null sell-cargo-at-contract-dest)
                 (task null navigate-to-market-dest)
                 (task null sell-cargo-at-market-dest)
                 (task null check-contract-status)
                 (task null mark-contract-fulfilled))))

(define (build-local-extract-loop-script ship-id system-id market-id)

  ;; 2023-06-27 djf - I removed the survey step, assuming we use a mining drone
  ;; fur this, which does not have a survey mount
  
  (define extract-from-current-location
    (λ (script-id state) (let-values ([(timestamp remaining-capacity) (extract ship-id)])
                           (printf "extract-from-current-location:  remaining capacity ~s~n" remaining-capacity)
                           (cond
                             [(zero? remaining-capacity) (task-result timestamp 'increment state)]
                             [else (task-result timestamp 'extract state)]))))
    
  (define sell-cargo-at-market
    (λ (script-id state) (let ([timestamp (sell ship-id system-id market-id)])
                           (task-result timestamp 'increment state)))) 
  
  (define jettison-all-cargo
    (λ (script-id state) (let ([timestamp (jettison ship-id)])
                           (task-result timestamp 'increment state)))) 
  
  (list->vector (list
                 (task 'repeat jettison-all-cargo)
                 (task 'extract extract-from-current-location)
                 (task null sell-cargo-at-market)
                 (task null check-count))))

(define (run-navigate-test)
  (define queue (make-queue))
  (define scripts (make-hash))
  (define state (hash 'count 0 'max-count 3))
  
  (hash-set! scripts 'ship-1-script-id (build-navigate-script "DRFOGOUT-1" "X1-HQ18-98695F"))
  (hash-set! scripts 'ship-2-script-id (build-navigate-script "DRFOGOUT-2" "X1-HQ18-98695F"))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'ship-1-script-id 0) state))

  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'ship-2-script-id 0) state))

  (process-queue scripts queue))

(define (run-negotiate-test)
  (define queue (make-queue))
  (define scripts (make-hash))
  
  (hash-set! scripts 'negotiate (build-negotiate-script "DRFOGOUT-1"))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'negotiate 0) (hash)))

  (process-queue scripts queue))

(define (run-extract-loop [ship-id "DRFOGOUT-3"])
  (define queue (make-queue))
  
  (printf "start extract loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent))))
  
  (define extract-script (build-extract-loop-script ship-id
                                                    asteroid-field-id
                                                    system-id
                                                    market-dest-id
                                                    asteroid-field-id))
  (define scripts (hash 'extract extract-script))
  (define state (hash 'count 0 'max-count 10))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'extract 0) state))

  (process-queue scripts queue)
  
  (printf "finish extract loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent)))))

(define (run-contract-loop contract-id [ship-id "DRFOGOUT-1"])
  (printf "start contract loop: ~s; ~s~n" contract-id ship-id)
  
  (define queue (make-queue))
  (define contract-loop-script (build-contract-loop-script contract-id
                                                           contract-cargo-id
                                                           ship-id
                                                           asteroid-field-id
                                                           system-id
                                                           contract-dest-id
                                                           market-dest-id))
  
  (define scripts (hash 'contract-loop contract-loop-script))
  (define state (hash))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'contract-loop 0) state))

  (process-queue scripts queue)
  
  (printf "finish contract loop: ~s; ~s~n" contract-id ship-id))
  
(define (run-extract-local-loop [ship-id "DRFOGOUT-1"] [max-count 30])
  (printf "start extract local loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent))))
  
  (define queue (make-queue))
  (define extract-script (build-local-extract-loop-script ship-id
                                                          system-id
                                                          asteroid-field-id))
  (define scripts (hash 'extract extract-script))
  (define state (hash 'count 0 'max-count max-count))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'extract 0) state))

  (process-queue scripts queue)
  
  (printf "finish local extract loop: ship: ~s; credits ~s~n" ship-id (agent-credits (data (get-agent)))))

;; navigate the ship to the specified location
;; do nothing if the ship is already there
(define (navigate ship-id destination-id)
  (dock-ship ship-id)
  (refuel-ship ship-id)  
  (orbit-ship ship-id)
  
  (let ([current-location (ship-location (data (get-ship ship-id)))])
    (printf "navigate ~s from ~s to ~s~n" ship-id current-location destination-id)
    (cond
      [(equal? current-location destination-id)
       (current-utc-date)]
      [else
       (let ([nav-result (navigate-ship ship-id destination-id)])
         (parse-timestamp (nav-arrival (data nav-result))))])))
      
;; assume ship is at headquarters
;; dock
;; negotiate-contract
(define (negotiate ship-id)
  (dock-ship ship-id)
  (refuel-ship ship-id)  
  
  (printf "negotiate contract ~s~n" ship-id)
  (let ([result (negotiate-contract ship-id)])
    (printf "contract result: ~n~s~n" result))
  (current-utc-date))

(define (fulfill contract-id)
  (printf "fullfill contract ~s~n" contract-id)
  (let ([result (fulfill-contract contract-id)])
    (printf "contract result: ~n~s~n" result))
  (current-utc-date))

;; extract resources from a suitable location
(define (extract ship-id)
  (orbit-ship ship-id)
  
  (let* ([ship-details (get-ship ship-id)]
         [ship-detail-data (data ship-details)]
         [starting-capacity (ship-cargo-capacity ship-detail-data)]
         [starting-units (ship-cargo-units ship-detail-data)])
    (cond
      [(>= starting-units starting-capacity)
       (values (current-utc-date) 0)]
      [else
       (printf "extract: ~s~n" ship-id)
       (let* ([extract-result (extract-resources ship-id)]
              [extract-result-data (data extract-result)]
              [expiration (parse-timestamp (cooldown-expiration extract-result-data))]
              [capacity (cargo-capacity extract-result-data)]
              [units (cargo-units extract-result-data)]
              [remaining-capacity (- capacity units)])
         (values expiration remaining-capacity))])))

(define (extract-total-price sell-result)
  (~> sell-result
      (hash-ref 'data)
      (hash-ref 'transaction)
      (hash-ref 'totalPrice)))

;; sell the ship's cargo
(define (sell ship-id system-id waypoint-id)
  (dock-ship ship-id)
  
  (let ([market-trade-goods (get-market-trade-goods system-id waypoint-id)]
        [inventory (ship-inventory (data (get-ship ship-id)))])
    (printf "sell: trade goods at market: ~s ~s: ~s\n"
            system-id
            waypoint-id
            (hash-keys market-trade-goods))
    (printf "sell: ship inventory: ~s: ~s~n" ship-id (map symbol inventory))
    (for ([item (in-list inventory)])
      (let ([symbol (hash-ref item 'symbol)]
            [units (hash-ref item 'units)])
        (cond
          [(hash-has-key? market-trade-goods symbol)
           (let* ([sell-result (sell-cargo ship-id symbol units)]
                  [price (extract-total-price sell-result)])
             (printf "sell: selling ~s units of ~s for ~s~n" units symbol price))
           #t]
          [else 
           (printf "sell: ignoring ~s units of ~s~n" units symbol)
           #f]))))
  
  (refuel-ship ship-id)
  (orbit-ship ship-id)
  
  (current-utc-date))
          
;; deliver the ship's contract cargo
(define (deliver  contract-id ship-id cargo-symbol)
  (dock-ship ship-id)
  
  (let ([inventory (ship-inventory (data (get-ship ship-id)))])
    (printf "deliver: contract: ~s; inventory: ~s~n" contract-id (map symbol inventory))
    (for ([item (in-list inventory)])
      (let ([symbol (hash-ref item 'symbol)]
            [units (hash-ref item 'units)])
        (cond
          [(equal? symbol cargo-symbol)
           (deliver-contract contract-id ship-id cargo-symbol units)
           (printf "deliver: contract: ~s; delivering ~s units of ~s~n"
                   contract-id units symbol)]
          [else 
           (printf "deliver: contract: ~s; ignoring ~s units of ~s~n" contract-id units symbol)
           #f]))))
  
  
  (refuel-ship ship-id)
  (orbit-ship ship-id)
  
  (current-utc-date))

(define (compute-units-needed contract-id trade-good)
  (let* ([contract (data (get-contract contract-id))]
         [deliverables (contract-deliverables contract)]
         [deliverable (findf (λ (elem) (equal? (deliverable-trade-symbol elem) trade-good))
                             deliverables)]
         [units-required (deliverable-units-required deliverable)]
         [units-fulfilled (deliverable-units-fulfilled deliverable)])
    (printf "compute-units-needed: ~s ~s required: ~s; fulfilled ~s~n"
            contract-id trade-good units-required units-fulfilled)
    (let ([units-needed
           (cond
             [(>= units-fulfilled units-required) 0]
             [else (- units-required units-fulfilled)])])
      (values (current-utc-date) units-needed))))
    
         
          
;; jettison the ship's (unsold) cargo
(define (jettison ship-id)
  (let ([inventory (ship-inventory (data (get-ship ship-id)))])
    (for ([item (in-list inventory)])
      (let ([symbol (hash-ref item 'symbol)]
            [units (hash-ref item 'units)])
        (printf "jettison: ship: ~s; ~s units of ~s~n" ship-id units symbol)
        (jettison-cargo ship-id symbol units))))
             
  (current-utc-date))

(define (survey-expired? survey)
  (let* ([survey-expiration (hash-ref survey 'expiration)]
         [survey-expiration-date (parse-timestamp survey-expiration)]
         [survey-expiration-seconds (date->seconds survey-expiration-date #f)])
    (< survey-expiration-seconds (current-seconds))))         

;; run a survey if there isn't a current one
(define (survey state ship-id)
  ;; TODO: generate a unique key using format and string->symbol
  (let* ([survey-key 'survey]
         [survey (hash-ref state survey-key #f)])
    (cond
      [(or (not survey) (survey-expired? survey))
       (let* ([survey-result (create-survey ship-id)]
              [survey-result-data (data survey-result)]
              [surveys (hash-ref survey-result-data 'surveys)]
              [expiration (parse-timestamp (cooldown-expiration survey-result-data))])
         (printf "survey: ship: ~s: ~s~n" ship-id surveys)
         (values expiration (hash-set state survey-key (first surveys))))]
      [else (values (current-utc-date) state)])))
       
    
