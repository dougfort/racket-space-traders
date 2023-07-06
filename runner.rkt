#lang racket

;; 

(provide run)

(require space-traders-v2/access)

(define (read-access-token)
  (define access-token-path "access-token.txt")
  (string-trim (port->string (open-input-file access-token-path) #:close? #t)))

(define (run fn)
  (parameterize ([access-token (read-access-token)])
    (fn)))

