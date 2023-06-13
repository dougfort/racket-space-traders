#lang racket

;; 

(provide waypoint-symbol
         waypoint-type
         waypoint-traits)

(require threading)

(define (waypoint-symbol waypoint-details)
  (~> waypoint-details
      (hash-ref 'symbol)))

(define (waypoint-type waypoint-details)
  (~> waypoint-details
      (hash-ref 'type)))

(define (waypoint-traits waypoint-details)
  (~> waypoint-details
      (hash-ref 'traits)))