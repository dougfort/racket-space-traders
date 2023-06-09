#lang racket

;; 

(require net/http-client)
(require json)
(require "access.rkt")

(provide api-get api-post api-patch)

(define http-too-many-requests 429)
(define retry-delay 1)

;; looking for '("HTTP/1.1" "200" "OK")
;; returning (status reason)
(define (parse-status-line status-line)
  (let ([parts ((compose1 string-split bytes->string/utf-8) status-line)])
    (values ((compose string->number second) parts) (string-join (drop parts 2)))))

;; Authorization: Bearer <access-token>
(define (create-auth-header)
  (unless access-token (load-access-token))
  (string-append "Authorization: "
                 (string-append "Bearer " access-token)))

;; generic HTTP GET of URI
(define (api-get uri [expected-status (list 200)])
  (let-values ([(status-line header-list data-port)
                (http-sendrecv
                 "api.spacetraders.io"
                 uri
                 #:ssl? #t
                 #:method #"GET"
                 #:headers (list (create-auth-header)))])
    (let-values ([(status reason) (parse-status-line status-line)])
      (let ([body (read-json data-port)])
        (cond
          [(member status expected-status) body]
          [(equal? status http-too-many-requests)
           (printf "Retry: status ~s; ~s; delay ~s~n" status reason retry-delay)
           (sleep retry-delay)
           ;; TODO use the dealy from Retry-After
           ;; Have some limit on retries
           (api-get uri expected-status)]
          [else
           (error (format "invalid HTTP status ~s; ~s; ~s~n~s"status reason uri body))])))))

;; generic HTTP POST of URI and JSON data
(define (api-post uri data [expected-status (list 200)])
  (let ([post-data (cond
                     [(hash? data) (jsexpr->string data)]
                     [else data])])
    (let-values ([(status-line header-list data-port)
                  (http-sendrecv
                   "api.spacetraders.io"
                   uri
                   #:ssl? #t
                   #:method #"POST"
                   #:headers (list (create-auth-header)
                                   "Content-Type: application/json")
                   #:data post-data)])
      (let-values ([(status reason) (parse-status-line status-line)])
        (let ([body (read-json data-port)])
          (cond
            [(member status expected-status) body]
            [(equal? status http-too-many-requests)
             (printf "Retry: status ~s; ~s; delay ~s~n" status reason retry-delay)
             (sleep retry-delay)
             ;; TODO use the dealy from Retry-After
             ;; Have some limit on retries
             (api-post uri data expected-status)]
            [else
             (error (format "invalid HTTP status ~s; ~s; ~s~n~s"status reason uri body))]))))))

;; generic HTTP PATCH of URI and JSON data
(define (api-patch uri data [expected-status (list 200)])
  (let ([patch-data (cond
                      [(hash? data) (jsexpr->string data)]
                      [else data])])
    (let-values ([(status-line header-list data-port)
                  (http-sendrecv
                   "api.spacetraders.io"
                   uri
                   #:ssl? #t
                   #:method #"PATCH"
                   #:headers (list (create-auth-header)
                                   "Content-Type: application/json")
                   #:data patch-data)])
      (let-values ([(status reason) (parse-status-line status-line)])
        (let ([body (read-json data-port)])
          (cond
            [(member status expected-status) body]
            [(equal? status http-too-many-requests)
             (printf "Retry: status ~s; ~s; delay ~s~n" status reason retry-delay)
             (sleep retry-delay)
             ;; TODO use the dealy from Retry-After
             ;; Have some limit on retries
             (api-patch uri data expected-status)]
            [else
             (error (format "invalid HTTP status ~s; ~s; ~s~n~s"status reason uri body))]))))))
