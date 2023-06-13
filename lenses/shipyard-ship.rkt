#lang racket

;; 

(provide shipyard-ship-type shipyard-ship-name shipyard-ship-purchase-price)

(require threading)

(define (shipyard-ship-type details)
  (hash-ref details 'type))

(define (shipyard-ship-name details)
  (hash-ref details 'name))

(define (shipyard-ship-purchase-price details)
  (hash-ref details 'purchasePrice))

