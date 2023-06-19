#lang racket

;; 

(provide deliverable-trade-symbol deliverable-units-required deliverable-units-fulfilled)

(define (deliverable-trade-symbol deliverable)
  (hash-ref deliverable 'tradeSymbol))

(define (deliverable-units-required deliverable)
  (hash-ref deliverable 'unitsRequired))

(define (deliverable-units-fulfilled deliverable)
  (hash-ref deliverable 'unitsFulfilled))

