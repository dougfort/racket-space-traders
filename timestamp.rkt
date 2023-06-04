#lang racket

;; 

(require racket/date)

(provide parse-timestamp)

;; take a string of the form "2019-08-24T14:15:22Z"
;; and return a Racket date
;; I'm writing this myself because 'gregor' doesn't produce a real date
;; and I only want to parse the timestamps from the Space Traders API
(define (parse-timestamp str)
  (seconds->date (find-seconds (string->number  (substring str 17 19))
                               (string->number  (substring str 14 16))
                               (string->number  (substring str 11 13))
                               (string->number  (substring str 8 10))
                               (string->number  (substring str 5 7))
                               (string->number  (substring str 0 4)))
                 #t))
