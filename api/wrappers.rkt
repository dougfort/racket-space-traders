#lang racket

;; 

;; outer JSON tags from API HTTP calls

(provide data meta)

(define (data object)
  (hash-ref object 'data))

(define (meta object)
  (hash-ref object 'meta))
  