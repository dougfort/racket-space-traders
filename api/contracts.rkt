#lang racket

;; 

;; This module implements the 'contracts' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-contracts
         get-contract
         accept-contract
         deliver-contract
         fulfill-contract)

(require "http.rkt")

;; List all of your contracts.
(define (list-contracts [limit 10] [page 1])
  ;; TODO: implement parameters
  (hash-ref (api-get "/v2/my/contracts") 'data))

;; Get the details of a contract by ID.
(define (get-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id) "")])
    (hash-ref (api-get uri) 'data)))

;; Accept a contract.
(define (accept-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/accept") "")])
    (api-post uri #f)))

;; Deliver cargo on a given contract.
(define (deliver-contract contract-id ship-symbol cargo-symbol units)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/deliver") "")]
        [data (hash 'shipSymbol ship-symbol 'tradeSymbol cargo-symbol 'units units)])
    (hash-ref (api-post uri data) 'data))) 

;; Fulfill a contract
(define (fulfill-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/fulfill") "")])
    (hash-ref (api-post uri #f) 'data))) 

                       