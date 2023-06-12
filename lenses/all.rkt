#lang racket

;; 

;; general purpose lenses that apply to all objects

(provide symbol)

(define (symbol x)
  (hash-ref x 'symbol))

