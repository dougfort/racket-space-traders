#lang racket

;; 

(require data/heap)
(require racket/date)

(provide make-queue
         queue-add!
         queue-add-until!
         queue-empty?
         queue-wait
         queue-pop)

(define date-string "2019-08-24T14:15:22Z")

;; a priority queue based on current time in seconds since the epoch

(struct entry (secs payload))

(define (entry<=? x y)
  (<= (entry-secs x) (entry-secs y)))

(define (make-queue)
  (make-heap entry<=?))

(define (queue-add! q wait-secs payload)
  (heap-add! q (entry (+ (current-seconds) wait-secs) payload)))

(define (queue-add-until! q deadline payload)
  (heap-add! q (entry deadline payload)))

(define (queue-empty? q)
  (zero? (heap-count  q)))

;; wait until the wait time on the head of the queue has passed
(define (queue-wait q)
  (let ([delta (- (entry-secs (heap-min q)) (current-seconds))])
    (cond
      [(<= delta 0) #t]
      [else
       (printf "queue-wait sleeping ~a seconds~n" delta)
       (sleep delta)])))

;; remove and return head of the queue
(define (queue-pop q)
  (let* ([e (heap-min q)]
         [delta (- (entry-secs e) (current-seconds))])
    (cond
      [(<= delta 0) (entry-payload e)]
      [else (error "delay has not finished ~s" e)])))
