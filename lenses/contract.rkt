#lang racket

;; 

(provide contract-deliverables)

(require threading)

(define (contract-id contract)
  (hash-ref contract 'id))

(define (contract-deliverables contract)
  (~> contract
      (hash-ref 'terms)
      (hash-ref 'deliver)))