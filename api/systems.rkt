#lang racket

;; 

;; This module implements the 'systems' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-systems
         get-system
         list-waypoints-in-system
         get-waypoint
         get-market
         get-shipyard
         get-jump-gate)

(require "http.rkt")

;; Return a list of all systems.
;; limit 0 means use the system defaults
(define (list-systems [limit 0] [page 1])
  (let* ([path "/v2/systems"]
         [query (limit-query-string limit page)]
         [uri (string-join (list path query) "")])
    (api-get uri)))

;; Get the details of a system.
(define (get-system system-symbol)  
  (let ([uri (string-join (list "/v2/systems/" system-symbol) "")])
    (api-get uri)))

;; Return a paginated list of all of the waypoints for a given system
;;
;; If a waypoint is uncharted, it will return the Uncharted trait instead of its actual traits.
(define (list-waypoints-in-system system-symbol [limit 0] [page 1])
  (let* ([path (string-join (list "/v2/systems/" system-symbol "/waypoints") "")]
         [query (limit-query-string limit page)]
         [uri (string-join (list path query) "")])
    (api-get uri)))

;; View the details of a waypoint.
;;
;; If the waypoint is uncharted, it will return the 'Uncharted' trait instead of its actual traits.
(define (get-waypoint system-symbol waypoint-symbol)  
  (let ([uri (string-join (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol) "")])
    (api-get uri)))

;; Retrieve imports, exports and exchange data from a marketplace.
;; Requires a waypoint that has the Marketplace trait to use.
;;
;; Send a ship to the waypoint to access trade good prices and recent transactions.
;; Refer to the Market Overview page to gain better a understanding of the market in the game.
(define (get-market system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/market") "")])
    (api-get uri)))

;; Get the shipyard for a waypoint.
;; Requires a waypoint that has the Shipyard trait to use.
;; Send a ship to the waypoint to access data on ships that are currently available
;; for purchase and recent transactions.
(define (get-shipyard system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/shipyard") "")])
    (api-get uri)))

;; Get jump gate details for a waypoint. Requires a waypoint of type JUMP_GATE to use.
;;
;; The response will return all systems that are have a Jump Gate in range of this Jump Gate.
;; Those systems can be jumped to from this Jump Gate.
(define (get-jump-gate system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/jump-gate") "")])
    (api-get uri)))

