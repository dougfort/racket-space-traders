#lang racket

;; 

(require threading)
(require "timestamp.rkt")
(require "api/systems.rkt")
(require "api/fleet.rkt")
(require "api/contracts.rkt")
(require "api/agents.rkt")
(require "api/factions.rkt")

;; extract system id from (current) waypoint-id
(define (extract-system-id waypoint-id)
  (string-join (take (string-split waypoint-id "-") 2) "-"))

(define (list-system-by-key key)
  (map (λ (s) (hash-ref s key)) (list-systems)))

(define (list-waypoint-symbols system-id)
  (map (λ (wp) (hash-ref wp 'symbol)) (list-waypoints-in-system system-id)))

(define (list-waypoints-traits system-id)
  (for ([wp (list-waypoints-in-system system-id)])
    (printf "symbol: ~a; type: ~a~n" (hash-ref wp 'symbol) (hash-ref wp 'type))
    (for ([t (hash-ref wp 'traits)])
      (printf "    ~a~n" (hash-ref t 'symbol)))))

(define (waypoint-has-trait? waypoint-details trait-symbol)
  (memf (λ (t) (equal? (hash-ref t 'symbol) trait-symbol)) (hash-ref waypoint-details 'traits)))

(define (waypoint-type waypoint-details)
  (hash-ref waypoint-details 'type))

(define (list-waypoints-with-shipyard system-id)
  (filter (λ (wp) (waypoint-has-trait? wp "SHIPYARD")) (list-waypoints-in-system system-id)))

(define (list-asteroid-field-waypoints system-id)
  (filter (λ (wp) (equal? (waypoint-type wp) "ASTEROID_FIELD")) (list-waypoints-in-system system-id)))

(define (list-marketplace-waypoint-symbols system-id)
  (map extract-symbol
       (filter (λ (wp) (waypoint-has-trait? wp "MARKETPLACE")) (list-waypoints-in-system system-id))))

(define (list-waypoint-market-trade-goods waypoint-id)
  (hash-ref (get-market waypoint-id) 'tradeGoods))
    
(define (list-waypoint-market-exports waypoint-id)
  (hash-ref (get-market (extract-system-id waypoint-id) waypoint-id) 'exports))
    
(define (list-waypoint-market-export-symbols waypoint-id)
  (map (λ (x) (hash-ref x 'symbol)) (list-waypoint-market-exports waypoint-id)))

(define (display-all-waypoint-markets system-id)
  (map (λ (mp) (display-market system-id mp)) (list-marketplace-waypoint-symbols system-id)))

(define (ship-status ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'nav)
      (hash-ref 'status)))

(define (ship-location ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'nav)
      (hash-ref 'waypointSymbol)))

(define (ship-inventory ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'inventory)))

(define (ship-inventory-units inventory trade-symbol)
  (for/or ([item inventory])
    (cond
      [(equal? (hash-ref item 'symbol) trade-symbol) (hash-ref item 'units)]
      [else #f])))

(define (ship-cargo-capacity ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'capacity)))

(define (ship-cargo-units ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'cargo)
      (hash-ref 'units)))

(define (ship-current-fuel ship-symbol)
  (~> (get-ship ship-symbol)
      (hash-ref 'fuel)
      (hash-ref 'current)))

(define (extract-result-capacity extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'capacity)))
         
(define (extract-result-units extract-result)
  (~> extract-result
      (hash-ref 'cargo)
      (hash-ref 'units)))
         
(define (cooldown-expiration result)
  (parse-timestamp (~> result
                       (hash-ref 'cooldown)
                       (hash-ref 'expiration))))  
         
(define (nav-result-arrival nav-result)
  (parse-timestamp (~> nav-result
                       (hash-ref 'nav)
                       (hash-ref 'route)
                       (hash-ref 'arrival))))

(define (list-ship-inventory ship-symbol)
  (for/list ([item (ship-inventory ship-symbol)])
    (cons (hash-ref item 'symbol) (hash-ref item 'units))))

(define (inventory-amount trade-symbol ship-inventory)
  (let ([xs (filter (λ (x) (equal? (car x) trade-symbol)) ship-inventory)])
    (cond
      [(null? xs) 0]
      [(equal? (length xs) 1) (cdr (first xs))]
      [else (error (format "invalid inventory ~s;" ship-inventory))])))
  
(define (ship-has-survery-mount? ship-symbol)
  (let ([mounts (~> (get-ship ship-symbol)
                    (hash-ref 'mounts))])
    (findf (λ (m) (string-prefix? (hash-ref m 'symbol) "MOUNT_SURVEYOR")) mounts)))                


(define (list-contract-deliverables contract-id)
  (~> (get-contract contract-id)
      (hash-ref 'terms)
      (hash-ref 'deliver)))

(define (contract-deliverable contract trade-symbol)
  (let ([deliverables (~>
                       contract
                       (hash-ref 'terms)
                       (hash-ref 'deliver))])
    (findf (λ (d) (equal? (hash-ref d 'tradeSymbol) trade-symbol)) deliverables)))
                      
(define (agent-headquarters agent-details)
  (~> agent-details
      (hash-ref 'data)
      (hash-ref 'headquarters)))

(define (agent-credits agent-details)
  (~> agent-details
      (hash-ref 'data)
      (hash-ref 'credits)))

(define (extract-symbol x)
  (hash-ref x 'symbol))

(define (display-market system-id waypoint-id)
  (let* ([market (get-market system-id waypoint-id)]
         [imports (hash-ref market 'imports)]
         [exports (hash-ref market 'exports)]
         [exchange (hash-ref market 'exchange)]
         [trade-goods (hash-ref market 'tradeGoods #f)])
    (printf "waypoint: ~s~n" (hash-ref market 'symbol))
    (printf "    imports: ~s~n" (map extract-symbol imports))
    (printf "    exports: ~s~n" (map extract-symbol exports))
    (printf "    exchange: ~s~n" (map extract-symbol exchange))
    (when trade-goods
      (printf "    trade goods~n")
      (for ([tg (in-list trade-goods)])
        (printf "        ~s: purchase: ~s; sell: ~s~n"
                (hash-ref tg 'symbol)
                (hash-ref tg 'purchasePrice)
                (hash-ref tg 'sellPrice))))))

(define (display-all-markets system-id)
  (map (λ (mp) (display-market system-id mp)) (list-marketplace-waypoint-symbols system-id)))

(define (display-ship ship)
  (printf "name: ~s; type: ~s; price: ~s~n"
          (hash-ref ship 'name)
          (hash-ref ship 'type)
          (hash-ref ship 'purchasePrice)))

(define (list-recruiting-factions)
  (map extract-symbol (filter (λ (f) (hash-ref f 'isRecruiting)) (list-factions))))