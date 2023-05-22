#lang racket

;; 

(require threading)
(require "api.rkt")

(provide get-agent-details
         agent-headquarters)

(define (get-agent-details)
  (hash-ref (api-get "/v2/my/agent") 'data))

(define (agent-headquarters)
  (~> (get-agent-details)
      (hash-ref 'headquarters)))
