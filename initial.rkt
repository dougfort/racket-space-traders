#lang racket

;; 

(provide initial-scan)

(require space-traders-v2/fleet)
(require space-traders-v2/systems)
(require space-traders-v2/wrappers)

(require "lenses/ship.rkt")
(require "lenses/waypoint.rkt")

(require "explore.rkt")
(require "runner.rkt")


(define (initial-scan state)
  (define command-ship (first (data (list-ships))))
  (define command-ship-symbol (hash-ref command-ship 'symbol))
  (define system-id (ship-system command-ship))
  (let ([state (hash-set* state
                          'command-ship-symbol command-ship-symbol
                          'system-id system-id)])
    (let ([state (load-waypoint-types state system-id)])
      (let ([state (load-markets state system-id)])
        state))))

(define (load-waypoint-types state system-id)
  (let loop ([waypoints (list-all-waypoints system-id)]
             [state state])
    (cond
      [(null? waypoints) state]
      [else
       (let ([wp (car waypoints)])
         (printf "~s ~s: ~s~n"
                 (waypoint-symbol wp)
                 (waypoint-type wp)
                 (map (λ (or) (hash-ref or 'symbol)) (hash-ref wp 'orbitals)))
         (cond
           [(equal? (waypoint-type wp) "ASTEROID_FIELD")
            (loop (cdr waypoints) (hash-set state 'asteroid-field (waypoint-symbol wp)))]
           [(equal? (waypoint-type wp) "JUMP_GATE")
            (loop (cdr waypoints) (hash-set state 'jump-gate (waypoint-symbol wp)))]
           [else (loop (cdr waypoints) state)]))])))
    
   
(define (load-markets state system-id)
  (let loop ([waypoint-ids (list-waypoints-with-trait system-id "MARKETPLACE")]
             [state (hash-set state 'exchange-markets '())])
    (cond
      [(null? waypoint-ids) state]
      [else
       (let* ([waypoint-id (car waypoint-ids)]
              [market (data (get-market system-id waypoint-id))]
              [exchange (hash-ref market 'exchange)])
         (cond
           [(> (length exchange) 2)
            (let ([exchange-markets (hash-ref state 'exchange-markets)]) 
              (loop (cdr waypoint-ids)
                    (hash-set state 'exchange-markets (cons waypoint-id exchange-markets))))] 
           [else (loop (cdr waypoint-ids) state)]))])))
       
