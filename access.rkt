#lang racket

;; 

(provide load-access-token access-token)

(define access-token #f)

(define (load-access-token)
  (set! access-token (string-trim (port->string (open-input-file "access-token.txt") #:close? #t))))
