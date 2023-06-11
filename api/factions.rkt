#lang racket

;; 

;; This module implements the 'factions' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-factions get-faction)

(require "http.rkt")

;; List all discovered factions in the game.
;; limit 0 means use the system defaults
(define (list-factions [limit 0] [page 1])
  (let* ([path "/v2/factions"]
         [query (limit-query-string limit page)]
         [uri (string-join (list path query) "")])
    (api-get uri)))

;; View the details of a faction.
(define (get-faction faction-symbol)
  (let ([uri (string-join (list "/v2/factions/" faction-symbol) "")])
    (api-get uri)))


