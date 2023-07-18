#lang racket

;; 

(provide make-queue
         queue-push-by-seconds
         queue-push-by-date
         task
         task-result
         script-pos
         process-queue)

(require racket/date)

(require "smol-queue.rkt")

(struct task (label fn))
(struct script-pos (id index))
(struct task-result (timestamp op state))

;; a priority queue based on current time in seconds since the epoch

(define (queue-push-by-seconds q wait-secs payload)
  (queue-push q (+ (current-seconds) wait-secs) payload))

(define (queue-push-by-date q deadline payload)
  (queue-push q (date->seconds deadline #f) payload))

;; wait until the wait time on the head of the queue has passed
(define (queue-wait q)
  (let ([delta (- (queue-peek-secs q) (current-seconds))])
    (cond
      [(<= delta 0) #t]
      [else
       (printf "queue-wait sleeping ~a seconds~n" delta)
       (sleep delta)])))

;; loop on a queue until it is empty
(define (process-queue scripts state queue)
  (cond
    [(queue-empty? queue) (println "queue is empty")]
    [else
     (queue-wait queue)
     (let-values ([(next-scripts next-state next-queue) (process-queue-entry scripts state queue)])
       (process-queue next-scripts next-state next-queue))]))

(define (process-queue-entry scripts state queue)
  (let-values ([(pos inner-queue) (queue-pop queue)])
    (let* ([script-id (script-pos-id pos)]
           [script-index (script-pos-index pos)]
           [script (hash-ref scripts script-id)])
      (cond
        [(>= script-index (vector-length script))
         (printf "end of script ~s~n" script-id)
         (values (hash-remove scripts script-id) state inner-queue)]
        [else
         (let* ([task-item (vector-ref script script-index)]
                [fn (task-fn task-item)]
                [result (fn state)]
                [timestamp (task-result-timestamp result)]
                [op (task-result-op result)]
                [index (cond
                         [(equal? op 'increment) (add1 script-index)]
                         [else (find-label-index script op)])])

           (values
            scripts
            (task-result-state result)
            (queue-push-by-date inner-queue
                                timestamp
                                (script-pos script-id index))))]))))
         
;; search a script vector
;; returning the index of a matching label
(define (find-label-index script label)
  (for/or ([i (in-naturals)])
    (let ([task-item (vector-ref script i)])
      (if (equal? (task-label task-item) label)
          i
          #f))))
            
