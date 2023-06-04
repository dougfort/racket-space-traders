#lang racket

;; 

;; This module implements the 'factions' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-factions get-faction)

(require "http.rkt")

;; List all discovered factions in the game.
(define (list-factions)
  (hash-ref (api-get "/v2/factions") 'data))

;; View the details of a faction.
(define (get-faction faction-symbol)
  (let ([uri (string-join (list "/v2/factions/" faction-symbol) "")])
    (hash-ref (api-get uri) 'data)))


