#lang racket

;; 

;; lenses into the cargo object

(provide cargo-capacity cargo-units)

(require threading)

(define (cargo-capacity result)
  (~> result
      (hash-ref 'cargo)
      (hash-ref 'capacity)))
         
(define (cargo-units result)
  (~> result
      (hash-ref 'cargo)
      (hash-ref 'units)))
         
