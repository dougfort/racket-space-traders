#lang racket

;; 

(require threading)
(require "api.rkt")

(define (list-factions)
  (hash-ref (api-get "/v2/factions") 'data))

(define (get-faction-details faction-symbol)
  (let ([uri (string-join (list "/v2/factions/" faction-symbol) "")])
    (hash-ref (api-get uri) 'data)))


