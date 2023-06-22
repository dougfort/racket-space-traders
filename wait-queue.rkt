#lang racket

;; 

(require data/heap)
(require racket/date)

(provide make-queue
         queue-push-by-seconds!
         queue-push-by-date!
         task
         task-step
         task-result
         script-pos
         process-queue)

(struct task (label fn))
(struct script-pos (id index))
(struct task-step (pos state)) 
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
(define (process-queue scripts queue)
  (cond
    [(queue-empty? queue) (println "queue is empty")]
    [else
     (queue-wait queue)
     (let* ([step (queue-pop queue)]
            [pos (task-step-pos step)]
            [state (task-step-state step)]
            [script-id (script-pos-id pos)]
            [script-index (script-pos-index pos)]
            [script (hash-ref scripts script-id)])
       (printf "script-id ~s; script-index ~s~n" script-id script-index )
       (cond
         [(>= script-index (vector-length script))
          (printf "end of script ~s~n" script-id)]
         [else
          (let* ([task-item (vector-ref script script-index)]
                 [fn (task-fn task-item)]
                 [result (fn script-id state)]
                 [timestamp (task-result-timestamp result)]
                 [op (task-result-op result)]
                 [state (task-result-state result)]
                 [index (cond
                          [(equal? op 'increment) (add1 script-index)]
                          [else (find-label-index script op)])])
                                                
            (queue-push-by-date! queue
                                 timestamp
                                 (task-step (script-pos script-id index) state)))])
       (process-queue scripts queue))]))

;; search a script vector
;; returning the index of a matching label
(define (find-label-index script label)
  (for/or ([i (in-naturals)])
    (let ([task-item (vector-ref script i)])
      (if (equal? (task-label task-item) label)
          i
          #f))))
            
