#lang racket

;; 

(require threading)
(require "api.rkt")

(provide list-contracts accept-contract list-contract-deliverables contract-deliver-cargo)

(define (list-contracts)
  (hash-ref (api-get "/v2/my/contracts") 'data))

(define (accept-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/accept") "")])
    (api-post uri #f)))

(define (get-contract-details contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id) "")])
    (hash-ref (api-get uri) 'data)))

(define (list-contract-deliverables contract-id)
  (~> (get-contract-details contract-id)
      (hash-ref 'terms)
      (hash-ref 'deliver)))

(define (contract-deliver-cargo contract-id ship-symbol trade-symbol units)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/deliver") "")]
        [data (hash 'shipSymbol ship-symbol 'tradeSymbol trade-symbol 'units units)])
    (api-post uri data))) 

