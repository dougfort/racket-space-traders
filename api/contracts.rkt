#lang racket

;; 

(require threading)
(require "http.rkt")

(provide list-contracts
         accept-contract
         list-contract-deliverables
         contract-deliver-cargo
         contract-deliverable)

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
    (hash-ref (api-post uri data) 'data))) 

(define (contract-deliverable contract trade-symbol)
  (let ([deliverables (~>
                       contract
                       (hash-ref 'terms)
                       (hash-ref 'deliver))])
    (findf (λ (d) (equal? (hash-ref d 'tradeSymbol) trade-symbol)) deliverables)))
                      
(define (contract-fulfill contract-id)
  (let ([uri (string-join (list "/v2/my/contracts/" contract-id "/fulfill") "")])
    (hash-ref (api-post uri #f) 'data))) 

                       