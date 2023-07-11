#lang racket

;; 

;; This module provides lenses for access into the agent object

(provide agent-symbol
         agent-headquarters
         agent-credits)

(require threading)

(define (agent-symbol agent-details)
  (~> agent-details
      (hash-ref 'symbol)))

(define (agent-headquarters agent-details)
  (~> agent-details
      (hash-ref 'headquarters)))

(define (agent-credits agent-details)
  (~> agent-details
      (hash-ref 'credits)))

