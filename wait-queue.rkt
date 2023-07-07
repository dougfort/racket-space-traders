#lang racket

;; 

(require data/heap)
(require racket/date)

(provide make-queue
         queue-push-by-seconds!
         queue-push-by-date!
         task
         task-result
         script-pos
         process-queue)

(struct task (label fn))
(struct script-pos (id index))
(struct task-result (timestamp op state))

;; a priority queue based on current time in seconds since the epoch

(struct entry (secs payload))

(define (entry<=? x y)
  (<= (entry-secs x) (entry-secs y)))

(define (make-queue)
  (make-heap entry<=?))

(define (queue-push-by-seconds! q wait-secs payload)
  (heap-add! q (entry (+ (current-seconds) wait-secs) payload)))

(define (queue-push-by-date! q deadline payload)
  (heap-add! q (entry (date->seconds deadline #f) payload)))

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
    (heap-remove-min! q)
    (cond
      [(<= delta 0) (entry-payload e)]
      [else (error "delay has not finished ~s" e)])))

;; loop on a queue until it is empty
(define (process-queue scripts state queue)
  (cond
    [(queue-empty? queue) (println "queue is empty")]
    [else
     (queue-wait queue)
     (let* ([pos (queue-pop queue)]
            [script-id (script-pos-id pos)]
            [script-index (script-pos-index pos)]
            [script (hash-ref scripts script-id)])
       (let ([next-state (cond
                           [(>= script-index (vector-length script))
                            (printf "end of script ~s~n" script-id)
                            state]
                           [else
                            (let* ([task-item (vector-ref script script-index)]
                                   [fn (task-fn task-item)]
                                   [result (fn state)]
                                   [timestamp (task-result-timestamp result)]
                                   [op (task-result-op result)]
                                   [index (cond
                                            [(equal? op 'increment) (add1 script-index)]
                                            [else (find-label-index script op)])])
                                                
                              (queue-push-by-date! queue
                                                   timestamp
                                                   (script-pos script-id index))
                              (task-result-state result))])])
         (process-queue scripts next-state queue)))]))

;; search a script vector
;; returning the index of a matching label
(define (find-label-index script label)
  (for/or ([i (in-naturals)])
    (let ([task-item (vector-ref script i)])
      (if (equal? (task-label task-item) label)
          i
          #f))))
            
