#lang racket

;; 

(provide display-waypoint-traits display-all-markets)

(require threading)
(require "timestamp.rkt")
(require "distance.rkt")
(require "api/systems.rkt")
(require "api/fleet.rkt")
(require "api/contracts.rkt")
(require "api/agents.rkt")
(require "api/factions.rkt")
(require "api/wrappers.rkt")
(require "lenses/all.rkt")
(require "lenses/agent.rkt")
(require "lenses/waypoint.rkt")
(require "lenses/ship.rkt")
(require "lenses/shipyard-ship.rkt")

(define (list-system-by-key key)
  (map (λ (s) (hash-ref s key)) (list-systems)))

(define (list-waypoint-symbols system-id)
  (map waypoint-symbol (data (list-waypoints-in-system system-id))))

;; load all the waypoint objects in a system into a hash for future reference
(define (load-waypoints-in-hash system-id)
  (for/hash ([waypoint (data (list-waypoints-in-system system-id))])
    (values (symbol waypoint) waypoint)))

;; return a list of pairs of (waypoint-id . distance) for all waypoints in a system
;; where distance is the distance from a specified waypoint
(define (list-waypoint-distances system-id waypoint-id)
  (let ([obj1 (data (get-waypoint system-id waypoint-id))])
    (map (λ (obj2) (cons (symbol obj2) (ceiling (distance obj1 obj2))))
         (data (list-waypoints-in-system system-id)))))

(define (display-waypoint-traits system-id)
  (for ([wp (data (list-waypoints-in-system system-id))])
    (printf "symbol: ~a; type: ~a~n" (waypoint-symbol wp) (waypoint-type wp))
    (for ([t (waypoint-traits wp)])
      (printf "    ~a~n" (hash-ref t 'symbol)))))

;; return the nammed trait or #f
;; intended to be used as a predicate
(define (waypoint-has-trait? waypoint-details trait-symbol)
  (findf (λ (t) (equal? (waypoint-symbol t) trait-symbol)) (waypoint-traits waypoint-details)))

(define (list-waypoints-with-shipyard system-id)
  (map waypoint-symbol
       (filter (λ (wp) (waypoint-has-trait? wp "SHIPYARD"))
               (data (list-waypoints-in-system system-id)))))

(define (list-asteroid-field-waypoints system-id)
  (map waypoint-symbol
       (filter (λ (wp) (equal? (waypoint-type wp) "ASTEROID_FIELD"))
               (data (list-waypoints-in-system system-id)))))

(define (list-marketplace-waypoint-symbols system-id)
  (map waypoint-symbol
       (filter (λ (wp) (waypoint-has-trait? wp "MARKETPLACE"))
               (data (list-waypoints-in-system system-id)))))

(define (list-waypoint-market-trade-goods waypoint-id)
  (hash-ref (get-market waypoint-id) 'tradeGoods))
    
(define (list-waypoint-market-exports system-id waypoint-id)
  (hash-ref (get-market system-id waypoint-id) 'exports))
    
(define (list-waypoint-market-export-symbols waypoint-id)
  (map (λ (x) (hash-ref x 'symbol)) (list-waypoint-market-exports waypoint-id)))

(define (display-all-waypoint-markets system-id)
  (map (λ (mp) (display-market system-id mp)) (list-marketplace-waypoint-symbols system-id)))

(define (display-shipyard-ship details)
  (printf "~s; ~s; ~s~n"
          (shipyard-ship-type details)
          (shipyard-ship-purchase-price details)
          (shipyard-ship-name details)))

(define (display-shipyard-ships shipyard)
  (for ([ship (in-list (hash-ref shipyard 'ships))])
    (display-shipyard-ship ship)))

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
                      
(define (display-market system-id waypoint-id)
  (let* ([market (data (get-market system-id waypoint-id))]
         [imports (hash-ref market 'imports)]
         [exports (hash-ref market 'exports)]
         [exchange (hash-ref market 'exchange)]
         [trade-goods (hash-ref market 'tradeGoods #f)])
    (printf "waypoint: ~s~n" (hash-ref market 'symbol))
    (printf "    imports: ~s~n" (map symbol imports))
    (printf "    exports: ~s~n" (map symbol exports))
    (printf "    exchange: ~s~n" (map symbol exchange))
    (when trade-goods
      (printf "    trade goods~n")
      (for ([tg (in-list trade-goods)])
        (printf "        ~s: purchase: ~s; sell: ~s~n"
                (hash-ref tg 'symbol)
                (hash-ref tg 'purchasePrice)
                (hash-ref tg 'sellPrice))))))

(define (display-all-markets system-id)
  (map (λ (mp) (display-market system-id mp)) (list-marketplace-waypoint-symbols system-id)))

(define (list-recruiting-factions)
  (map symbol (filter (λ (f) (hash-ref f 'isRecruiting)) (list-factions))))