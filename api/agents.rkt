#lang racket

;; 

;; This module implements the 'agents' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide my-agent-details)

(require "http.rkt")

;; Fetch your agent's details.
(define (my-agent-details)
  (hash-ref (api-get "/v2/my/agent") 'data))
