#lang racket

;; 

(require net/http-client)
(require json)

(define callsign "DRFOGOUT")
(define faction "COSMIC")

;; looking for '("HTTP/1.1" "200" "OK")
;; returning (status reason)
(define (parse-status-line status-line)
  (let ([parts ((compose1 string-split bytes->string/utf-8) status-line)])
    (values ((compose string->number second) parts) (string-join (drop parts 2)))))

(define (register-agent)
  (let ([uri "/v2/register"]
        [post-data (jsexpr->string (hash 'symbol callsign 'faction faction))])
    (let-values ([(status-line header-list data-port)
                  (http-sendrecv
                   "api.spacetraders.io"
                   uri
                   #:ssl? #t
                   #:method #"POST"
                   #:headers (list "Content-Type: application/json")
                   #:data post-data)])
      (let-values ([(status reason) (parse-status-line status-line)])
        (unless (= 201 status)
          (error (format "invalid HTTP status ~s; ~s; ~s" status reason uri))))
    (read-json data-port))))
