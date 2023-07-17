#lang racket

;; 

;; A small, inefficient priority queue based on a list
;; ordered by time in seconds since the epoch

(provide make-queue
         queue-empty?
         queue-push
         queue-peek-secs
         queue-pop)

(struct entry (secs payload))

(define (entry-less-than? lhs rhs)
  (< (entry-secs lhs) (entry-secs rhs)))

(define (make-queue)
  null)

(define (queue-push queue secs payload)
  (sort (cons (entry secs payload) queue) entry-less-than?))

(define (queue-empty? queue)
  (null? queue))

(define (queue-peek-secs queue)
  (entry-secs (car queue)))

(define (queue-pop queue)
  (values (entry-payload (car queue)) (cdr queue)))
  
(module+ test
  (require rackunit)
  (let ([q1 (make-queue)]) 
    (check-true (queue-empty? q1))
    (let ([q2 (queue-push q1 0 'payload)])
      (check-false (queue-empty? q2))
      (let-values ([(pl q3) (queue-pop q2)])
        (check-eq? pl 'payload)
        (check-true (queue-empty? q3))))))
