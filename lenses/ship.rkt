#lang racket

;; 

(provide ship-role
         ship-status
         ship-system
         ship-location
         ship-inventory
         ship-inventory-units
         ship-cargo-capacity
         ship-cargo-units
         ship-current-fuel)

;; a set of 'lenses' to pull out interesting peices of the complexly nested hash tables
;; of the ship object

(require threading)

(define (ship-role ship-details)
  (~> ship-details
      (hash-ref 'registration)
      (hash-ref 'role)))

(define (ship-status ship-details)
  (~> ship-details
      (hash-ref 'nav)
      (hash-ref 'status)))

(define (ship-system ship-details)
  (~> ship-details
      (hash-ref 'nav)
      (hash-ref 'systemSymbol)))

(define (ship-location ship-details)
  (~> ship-details
      (hash-ref 'nav)
      (hash-ref 'waypointSymbol)))

(define (ship-inventory ship-details)
  (~> ship-details
      (hash-ref 'cargo)
      (hash-ref 'inventory)))

(define (ship-inventory-units inventory trade-symbol)
  (let ([item (findf (λ (t) (equal? (hash-ref t 'symbol) trade-symbol)) inventory)])
    (if item
        (hash-ref item 'units)
        #f)))

(define (ship-cargo-capacity ship-details)
  (~> ship-details
      (hash-ref 'cargo)
      (hash-ref 'capacity)))

(define (ship-cargo-units ship-details)
  (~> ship-details
      (hash-ref 'cargo)
      (hash-ref 'units)))

(define (ship-current-fuel ship-details)
  (~> ship-details
      (hash-ref 'fuel)
      (hash-ref 'current)))