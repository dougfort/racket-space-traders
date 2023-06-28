#lang racket

;; 

(provide distance)

(define (distance obj1 obj2)
  (let* ([x1 (hash-ref obj1 'x)]
         [y1 (hash-ref obj1 'y)]
         [x2 (hash-ref obj2 'x)]
         [y2 (hash-ref obj2 'y)])
    (coord-distance x1 y1 x2 y2)))

(define (coord-distance x1 y1 x2 y2)
  (let* ([Δx (- x2 x1)]
         [Δy (- y2 y1)]
         [Δx-squared (* Δx Δx)]
         [Δy-squared (* Δy Δy)])
    (sqrt (+ Δx-squared Δy-squared))))
