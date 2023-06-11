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
;; limit 0 means use the system defaults
(define (list-contracts [limit 10] [page 1])
  (let* ([path "/v2/my/contracts"]
         [query (limit-query-string limit page)]
         [uri (string-join (list path query) "")])
    (api-get uri)))

;; Get the details of a contract by ID.
(define (get-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id) "")])
    (api-get uri)))

;; Accept a contract.
(define (accept-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/accept") "")])
    (api-post uri #f)))

;; Deliver cargo on a given contract.
(define (deliver-contract contract-id ship-symbol cargo-symbol units)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/deliver") "")]
        [data (hash 'shipSymbol ship-symbol 'tradeSymbol cargo-symbol 'units units)])
    (api-post uri data))) 

;; Fulfill a contract
(define (fulfill-contract contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/fulfill") "")])
    (api-post uri #f))) 

