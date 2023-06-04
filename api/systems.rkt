#lang racket

;; 

;; This module implements the 'systems' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-systems
         get-system
         list-waypoints
         get-waypoint
         get-market
         get-shipyard
         get-jump-gate)

(require "http.rkt")

;; Return a list of all systems.
(define (list-systems)
  (let ([uri "/v2/systems"])
    (hash-ref (api-get uri) 'data)))

;; Get the details of a system.
(define (get-system system-symbol)  
  (let ([uri (string-join (list "/v2/systems/" system-symbol) "")])
    (hash-ref (api-get uri) 'data)))

;; Fetch all of the waypoints for a given system.
;; System must be charted or a ship must be present to return waypoint details.
(define (list-waypoints system-symbol)
  (let ([uri (string-join (list "/v2/systems/" system-symbol "/waypoints") "")])
    (hash-ref (api-get uri) 'data)))

;; View the details of a waypoint.
(define (get-waypoint system-symbol waypoint-symbol)  
  (let ([uri (string-join (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol) "")])
    (hash-ref (api-get uri) 'data)))

;; Retrieve imports, exports and exchange data from a marketplace.
;; Imports can be sold, exports can be purchased, and exchange goods can be purchased or sold.
;; Send a ship to the waypoint to access trade good prices and recent transactions.
(define (get-market system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/market") "")])
    (hash-ref (api-get uri) 'data)))

;; Get the shipyard for a waypoint.
;; Send a ship to the waypoint to access ships that are currently available for purchase
;; and recent transactions.
(define (get-shipyard system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/shipyard") "")])
    (hash-ref (api-get uri) 'data)))

;; Get jump gate details for a waypoint.
(define (get-jump-gate system-symbol waypoint-symbol)  
  (let ([uri (string-join
              (list "/v2/systems/" system-symbol "/waypoints/" waypoint-symbol "/jump-gate") "")])
    (hash-ref (api-get uri) 'data)))

