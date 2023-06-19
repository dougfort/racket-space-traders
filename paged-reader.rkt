#lang racket

;; 

(provide flatten-pages) 

;; read APIs that are paged with 'limit' and 'page' arguments
;; return a list of the total content
(define (flatten-pages fn)
  (define limit 10)
  (let loop ([page 1]
             [accum '()])
    (let* ([result (fn limit page)]
           [meta (hash-ref result 'meta)]
           [total (hash-ref meta 'total)]
           [data (hash-ref result 'data)]
           [next-accum (append accum data)])
      (cond
        [(< (length next-accum) total) (loop (add1 page) next-accum)]
        [else next-accum]))))
        
