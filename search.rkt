#lang racket

;; 

(require "directory.rkt")
(require "explore.rkt")
(require "api/agents.rkt")
(require "api/systems.rkt")
(require "api/wrappers.rkt")
(require "lenses/agent.rkt")

(define (display-jump-gate-systems system-id waypoint-id)
  (let ([jump-gate-data (data (get-jump-gate system-id waypoint-id))])
    (printf "gate: ~s; range: ~s; faction: ~s~n"
            waypoint-id
            (hash-ref jump-gate-data 'jumpRange)
            (hash-ref jump-gate-data 'factionSymbol))
    (for ([system (in-list (hash-ref jump-gate-data 'connectedSystems))])
      (let* ([system-id (hash-ref system 'symbol)]
             [waypoints (list-all-waypoints system-id)])
        (printf "    ~s ~s ~s ~s ~s; ~s waypoints~n"
                system-id
                (hash-ref system 'sectorSymbol)
                (hash-ref system 'type)
                (hash-ref system 'factionSymbol)
                (hash-ref system 'distance)
                (length waypoints))))))

(define (filter-jump-gate-waypoints system-id waypoint-id pred?)
  (let* ([jump-gate-data (data (get-jump-gate system-id waypoint-id))]
         [systems (hash-ref jump-gate-data 'connectedSystems)]
         [systems-result (for/list ([system (in-list systems)])
                           (filter-system-waypoints (hash-ref system 'symbol) pred?))])
    (flatten systems-result)))

(define (filter-system-waypoints system-id pred?)
  (for/list ([waypoint (list-all-waypoints system-id)]
             #:when (pred? waypoint))
    waypoint))
    

(define (market-has-equipment? waypoint)
  (cond
    [(waypoint-has-trait? waypoint "MARKETPLACE")
     (let* ([system-id (hash-ref waypoint 'systemSymbol)]
            [waypoint-id (hash-ref waypoint 'symbol)]
            [market (get-market system-id waypoint-id)]
            [exchange (hash-ref (data market) 'exchange)])
       (findf (λ (item) (equal? (hash-ref item 'symbol) "EQUIPMENT")) exchange))]   
    [else #f]))

(define (market-has-fuel? waypoint)
  (cond
    [(waypoint-has-trait? waypoint "MARKETPLACE")
     (let* ([system-id (hash-ref waypoint 'systemSymbol)]
            [waypoint-id (hash-ref waypoint 'symbol)]
            [market (get-market system-id waypoint-id)]
            [exchange (hash-ref (data market) 'exchange)])
       (findf (λ (item) (equal? (hash-ref item 'symbol) "FUEL")) exchange))]   
    [else #f]))