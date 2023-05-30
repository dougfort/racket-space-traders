#lang racket

;; 

(provide load-access-token access-token)

(define access-token #f)

(define access-token-path "/home/dougfort/Development/racket-space-traders/access-token.txt")

(define (load-access-token)
  (set! access-token
        (string-trim (port->string (open-input-file access-token-path) #:close? #t))))
