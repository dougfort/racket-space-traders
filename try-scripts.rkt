#lang racket

;; 

(require threading)
(require racket/date)
(require "timestamp.rkt")
(require "api/agents.rkt")
(require "api/factions.rkt")
(require "api/fleet.rkt")
(require "api/systems.rkt")
(require "api/wrappers.rkt")
(require "lenses/agent.rkt")
(require "lenses/ship.rkt")
(require "lenses/ship-nav.rkt")
(require "lenses/cargo.rkt")
(require "lenses/cooldown.rkt")
(require "lenses/all.rkt")
(require "wait-queue.rkt")

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

(struct task (label fn))
(struct script-pos (id index))
(struct task-step (pos state)) 
(struct task-result (timestamp op state))

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
                           (printf "remaining capacity ~s~n" remaining-capacity)
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

(define (build-local-extract-loop-script ship-id system-id market-id)
  (define extract-from-current-location
    (λ (script-id state) (let-values ([(timestamp remaining-capacity) (extract ship-id)])
                           (printf "remaining capacity ~s~n" remaining-capacity)
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

(define (run-extract-loop-test [ship-id "DRFOGOUT-3"])
  (printf "start extract loop test: credits ~s~n" (agent-credits (data (get-agent))))
  
  (define system-id "X1-KS52")
  (define source-id "X1-KS52-51225B") ; asteroid field
  (define precious-ore-market-id "X1-KS52-25044Z")
  (define ore-market-id "X1-KS52-61262Z")
  (define queue (make-queue))
  (define extract-script (build-extract-loop-script ship-id
                                                    source-id
                                                    system-id
                                                    precious-ore-market-id
                                                    ore-market-id))
  (define scripts (hash 'extract extract-script))
  (define state (hash 'count 0 'max-count 10))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'extract 0) state))

  (process-queue scripts queue)
  
  (printf "finish extract loop test: credits ~s~n" (agent-credits (data (get-agent)))))

(define (run-extract-local-loop-test [ship-id "DRFOGOUT-1"])
  (printf "start extract local loop test: credits ~s~n" (agent-credits (data (get-agent))))
  
  (define system-id "X1-GX66")
  (define waypoint-id "X1-GX66-49714D")
  (define queue (make-queue))
  (define extract-script (build-local-extract-loop-script ship-id
                                                          system-id
                                                          waypoint-id))
  (define scripts (hash 'extract extract-script))
  (define state (hash 'count 0 'max-count 30))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'extract 0) state))

  (process-queue scripts queue)
  
  (printf "finish local extract loop test: credits ~s~n" (agent-credits (data (get-agent)))))

(define (process-queue scripts queue)
  (cond
    [(queue-empty? queue) (println "queue is empty")]
    [else
     (queue-wait queue)
     (let* ([step (queue-pop queue)]
            [pos (task-step-pos step)]
            [state (task-step-state step)]
            [script-id (script-pos-id pos)]
            [script-index (script-pos-index pos)]
            [script (hash-ref scripts script-id)])
       (printf "script-id ~s; script-index ~s~n" script-id script-index )
       (cond
         [(>= script-index (vector-length script))
          (printf "end of script ~s~n" script-id)]
         [else
          (let* ([task-item (vector-ref script script-index)]
                 [fn (task-fn task-item)]
                 [result (fn script-id state)]
                 [timestamp (task-result-timestamp result)]
                 [op (task-result-op result)]
                 [state (task-result-state result)]
                 [index (cond
                          [(equal? op 'increment) (add1 script-index)]
                          [else (find-label-index script op)])])
                                                
            (queue-push-by-date! queue
                                 timestamp
                                 (task-step (script-pos script-id index) state)))])
       (process-queue scripts queue))]))

;; search a script vector
;; returning the index of a matching label
(define (find-label-index script label)
  (for/or ([i (in-naturals)])
    (let ([task-item (vector-ref script i)])
      (if (equal? (task-label task-item) label)
          i
          #f))))
            
;; navigate the ship to the specified location
;; do nothing if the ship is already there
(define (navigate ship-symbol destination-symbol)
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  (orbit-ship ship-symbol)
  
  (printf "navigate ~s ~s~n" ship-symbol destination-symbol)
  (let ([current-location (ship-location (data (get-ship ship-symbol)))])
    (cond
      [(equal? current-location destination-symbol)
       (current-utc-date)]
      [else
       (let ([nav-result (navigate-ship ship-symbol destination-symbol)])
         (parse-timestamp (nav-arrival (data nav-result))))])))
      
;; assume ship is at headquarters
;; dock
;; negotiate-contract
(define (negotiate ship-symbol)
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  
  (printf "negotiate contract ~s~n" ship-symbol)
  (let ([result (negotiate-contract ship-symbol)])
    (printf "contract result: ~n~s~n" result)))

;; extract resources from a suitable location
(define (extract ship-symbol)
  (orbit-ship ship-symbol)
  
  (let* ([ship-details (get-ship ship-symbol)]
         [ship-detail-data (data ship-details)]
         [starting-capacity (ship-cargo-capacity ship-detail-data)]
         [starting-units (ship-cargo-units ship-detail-data)])
    (cond
      [(>= starting-units starting-capacity)
       (values (current-utc-date) 0)]
      [else
       (printf "extract resources ~s~n" ship-symbol)
       (let* ([extract-result (extract-resources ship-symbol)]
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
(define (sell ship-symbol system-id waypoint-id)
  (dock-ship ship-symbol)
  
  (let ([market-trade-goods (get-market-trade-goods system-id waypoint-id)]
        [inventory (ship-inventory (data (get-ship ship-symbol)))])
    (printf "trade goods at ~s ~s: ~s\n"
            system-id
            waypoint-id
            (hash-keys market-trade-goods))
    (printf "inventory: ~s~n" (map symbol inventory))
    (for ([item (in-list inventory)])
      (let ([symbol (hash-ref item 'symbol)]
            [units (hash-ref item 'units)])
        (cond
          [(hash-has-key? market-trade-goods symbol)
           (let* ([sell-result (sell-cargo ship-symbol symbol units)]
                  [price (extract-total-price sell-result)])
             (printf "selling ~s units of ~s for ~s~n" units symbol price))
           #t]
          [else 
           (printf "ignoring ~s units of ~a~n" units symbol)
           #f]))))
  
  (refuel-ship ship-symbol)
  (orbit-ship ship-symbol)
  
  (current-utc-date))
          
;; jettison the ship's (unsold) cargo
(define (jettison ship-symbol)
  (let ([inventory (ship-inventory (data (get-ship ship-symbol)))])
    (printf "jettisoning inventory: ~s~n" (map symbol inventory))
    (for ([item (in-list inventory)])
      (let ([symbol (hash-ref item 'symbol)]
            [units (hash-ref item 'units)])
        (printf "jettisoning ~s units of ~a~n" units symbol)
        (jettison-cargo ship-symbol symbol units))))
             
  (current-utc-date))

