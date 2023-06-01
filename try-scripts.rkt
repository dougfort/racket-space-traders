#lang racket

;; 

(require racket/date)
(require "api/agents.rkt")
(require "api/fleet.rkt")
(require "wait-queue.rkt")

(define (current-utc-date)
  (seconds->date (current-seconds) #f))

(define (make-state)
  (hash))

(struct task (label fn))
(struct script-pos (id index))
(struct script-next (script-id op))
(struct task-step (pos state)) 
(struct task-result (timestamp next state))

(define navigate-script-id 'navigate-script)


;; navigate to agent headquarters
;; then navige to the asteroid field and back
;; 3 times
(define (build-navigate-script)
  (define ship-id "REDSHIFT-1")
  (define asteroid-field "X1-NU19-72345Z")
  (define hq (agent-headquarters))
  (define max-count 3)
  (define navigate-to-hq
    (λ (script-id state) (let ([timestamp (navigate ship-id hq)])
                           (task-result timestamp (script-next script-id 'increment) state)))) 
  (define navigate-to-asteroid-field
    (λ (script-id state) (let ([timestamp (navigate ship-id asteroid-field)])
                           (task-result timestamp (script-next script-id 'increment) state)))) 
  (define check-count
    (λ (script-id state) (let ([timestamp (current-utc-date)]
                               [next-count (add1 (hash-ref state 'count 0))])
                           (cond                                
                             [(>= next-count max-count)
                              (task-result timestamp
                                           (script-next script-id 'increment)
                                           (hash-set state 'count 0))]
                             [else
                              (task-result timestamp
                                           (script-next script-id 'repeat)
                                           (hash-set state 'count next-count))]))))
    
     
  (list->vector (list
                 (task null navigate-to-hq)
                 (task 'repeat navigate-to-asteroid-field)
                 (task null navigate-to-hq)
                 (task null check-count))))
                                                                        

(define (run)
  (define queue (make-queue))
  (define scripts (make-hash))
  

  (hash-set! scripts navigate-script-id (build-navigate-script))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos navigate-script-id 0) (make-state)))

  (process-queue scripts queue))

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
       (printf "script-id ~s; script-index ~s; count ~s~n"
               script-id script-index (hash-ref state 'count 0))
       (cond
         [(>= script-index (vector-length script))
          (printf "end of script ~s~n" script-id)]
         [else
          (let* ([task-item (vector-ref script script-index)]
                 [fn (task-fn task-item)])
            (let* ([result (fn script-id state)]
                   [timestamp (task-result-timestamp result)]
                   [next (task-result-next result)]
                   [state (task-result-state result)]
                   [script-id (script-next-script-id next)]
                   [op (script-next-op next)])
              (let ([index (cond
                             [(equal? op 'increment) (add1 script-index)]
                             [else (find-label-index script op)])])
                                                
              (queue-push-by-date! queue
                                   timestamp
                                   (task-step (script-pos script-id index) state)))))])
       (process-queue scripts queue))]))

;; search a script vector
;; returning the index of a matching label
(define (find-label-index script label)
  (for/or ([i (in-naturals)])
    (let ([task-item (vector-ref script i)])
      (if (equal? (task-label task-item) label)
          i
          #f))))
            
;; navigate the ship to the specified location
;; do nothing if the ship is already there
(define (navigate ship-symbol destination-symbol)
  (printf "navigate ~s ~s~n" ship-symbol destination-symbol)
  (let ([current-location (ship-location ship-symbol)])
    (cond
      [(equal? current-location destination-symbol) (current-utc-date)]
      [else
       (let ([nav-result (navigate-ship ship-symbol destination-symbol)])
         (nav-result-arrival nav-result))])))
      
