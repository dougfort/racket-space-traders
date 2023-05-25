#lang racket

;; 

(require threading)
(require "api.rkt")

(provide extract-system-id
         list-waypoints-with-shipyard
         list-shipyard-ships
         purchase-ship
         list-asteroid-field-waypoints
         list-waypoint-market-trade-goods)

;; extract system id from (current) waypoint-id
(define (extract-system-id waypoint-id)
  (string-join (take (string-split waypoint-id "-") 2) "-"))

(define (list-waypoints system-id)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints") "")])
    (hash-ref (api-get uri) 'data)))

(define (get-waypoint-details waypoint-id)  
  (let* (
         [system-id (extract-system-id waypoint-id)]
         [uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id) "")])
    (hash-ref (api-get uri) 'data)))

(define (waypoint-has-trait? waypoint-details trait-symbol)
  (memf (λ (t) (equal? (hash-ref t 'symbol) trait-symbol)) (hash-ref waypoint-details 'traits)))

(define (waypoint-type waypoint-details)
  (hash-ref waypoint-details 'type))

(define (list-waypoints-with-shipyard system-id)
  (filter (λ (wp) (waypoint-has-trait? wp "SHIPYARD")) (list-waypoints system-id)))

(define (list-shipyard-ships system-id shipyard-id)
  (let ([uri (string-join (list "/v2/systems/" system-id "/waypoints/" shipyard-id "/shipyard") "")])
    (hash-ref (api-get uri) 'data)))
  
(define (purchase-ship ship-type shipyard-waypoint-symbol)
  (let ([uri "/v2/my/ships"]
        [data (hash 'shipType ship-type 'waypointSymbol shipyard-waypoint-symbol)])
    (api-post uri data 201)))

(define (list-asteroid-field-waypoints system-id)
  (filter (λ (wp) (equal? (waypoint-type wp) "ASTEROID_FIELD")) (list-waypoints system-id)))

(define (get-waypoint-market-data waypoint-id)
  (let* (
         [system-id (extract-system-id waypoint-id)]
         [uri (string-join (list "/v2/systems/" system-id "/waypoints/" waypoint-id "/market") "")])
    (hash-ref (api-get uri) 'data)))

(define (list-waypoint-market-trade-goods waypoint-id)
  (hash-ref (get-waypoint-market-data waypoint-id) 'tradeGoods))