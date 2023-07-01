#lang racket

;; 

(require space-traders-v2/http)
(require space-traders-v2/fleet)

(define (read-access-token)
  (define access-token-path "access-token.txt")
  (string-trim (port->string (open-input-file access-token-path) #:close? #t)))

(parameterize ([access-token (read-access-token)])
  (list-ships))
