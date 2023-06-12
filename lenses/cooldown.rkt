#lang racket

;; 

(provide cooldown-expiration)

(require threading)

(define (cooldown-expiration result)
  (~> result
      (hash-ref 'cooldown)
      (hash-ref 'expiration)))
         
