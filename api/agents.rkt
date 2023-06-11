#lang racket

;; 

;; This module implements the 'agents' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide get-agent)

(require "http.rkt")

;; Get Agent
(define (get-agent)
  (api-get "/v2/my/agent"))
