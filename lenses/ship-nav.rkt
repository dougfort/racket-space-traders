#lang racket

;; 

;; Lenses into the ShipNav object

(provide nav-arrival)

(require threading)

(define (nav-arrival nav-result)
  (~> nav-result
      (hash-ref 'nav)
      (hash-ref 'route)
      (hash-ref 'arrival)))

