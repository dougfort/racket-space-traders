#lang racket

;; 

(require net/http-client)
(require json)
(require threading)

(define token-file "access-token.txt")
(define expected-status 201)

;; looking for '("HTTP/1.1" "200" "OK")
;; returning (status reason)
(define (parse-status-line status-line)
  (let ([parts ((compose1 string-split bytes->string/utf-8) status-line)])
    (values ((compose string->number second) parts) (string-join (drop parts 2)))))

(define (register-new-agent callsign faction email)
  (let ([uri "/v2/register"]
        [post-data (jsexpr->string (hash 'symbol callsign 'faction faction 'email email))])
    (let-values ([(status-line header-list data-port)
                  (http-sendrecv
                   "api.spacetraders.io"
                   uri
                   #:ssl? #t
                   #:method #"POST"
                   #:headers (list "Content-Type: application/json")
                   #:data post-data)])
      (let-values ([(status reason) (parse-status-line status-line)])
        (let ([body (read-json data-port)])
          (cond
            [(= expected-status status) body]
            [else (error (format "invalid HTTP status ~s; ~s; ~s; ~s" status reason uri body))]))))))

(define (register)
  (define callsign "DRFOGOUT")
  ;(define faction "COSMIC")
  (define faction "QUANTUM")
  (define available-factions '("COSMIC"
                               "VOID"
                               "GALACTIC"
                               "QUANTUM"
                               "DOMINION"
                               "ASTRO"
                               "CORSAIRS"
                               "OBSIDIAN"
                               "AEGIS"
                               "UNITED"))
  (define email "doug.fort@gmail.com")

  (let* ([result (register-new-agent callsign faction email)]
         [token (~> result
                    (hash-ref 'data)
                    (hash-ref 'token))])
    (println token)
    (with-output-to-file token-file
      (λ () (printf token)))))
    
         
    
