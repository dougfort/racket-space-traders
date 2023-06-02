#lang racket

;; 

(require racket/date)
(require "api/agents.rkt")
(require "api/fleet.rkt")
(require "wait-queue.rkt")

(define (current-utc-date)
  (seconds->date (current-seconds) #f))

(define (increment-current-utc-date amount)
  (seconds->date (+ (current-seconds) amount) #f))

(define (make-state)
  (hash))

(struct task (label fn))
(struct script-pos (id index))
(struct task-step (pos state)) 
(struct task-result (timestamp op state))

;; navigate to agent headquarters
;; then navige to the asteroid field and back
;; 3 times
(define (build-navigate-script ship-id asteroid-field)
  (define hq (agent-headquarters))
  (define max-count 3)
  (define navigate-to-hq
    (λ (script-id state) (let ([timestamp (navigate ship-id hq)])
                           (task-result timestamp 'increment state)))) 
  (define navigate-to-asteroid-field
    (λ (script-id state) (let ([timestamp (navigate ship-id asteroid-field)])
                           (task-result timestamp 'increment state)))) 
  (define check-count
    (λ (script-id state) (let ([timestamp (current-utc-date)]
                               [next-count (add1 (hash-ref state 'count 0))])
                           (cond                                
                             [(>= next-count max-count)
                              (task-result timestamp 'increment (hash-set state 'count 0))]
                             [else
                              (task-result timestamp 'repeat (hash-set state 'count next-count))]))))
    
     
  (list->vector (list
                 (task null navigate-to-hq)
                 (task 'repeat navigate-to-asteroid-field)
                 (task null navigate-to-hq)
                 (task null check-count))))

(define (build-negotiate-script ship-id)
  (define hq (agent-headquarters))
  (define navigate-to-hq
    (λ (script-id state) (let ([timestamp (navigate ship-id hq)])
                           (task-result timestamp 'increment state)))) 
  (define negotiate
    (λ (script-id state) (let ([timestamp (current-utc-date)])
                           (negotiate-contract ship-id)
                           (task-result timestamp 'increment state))))      
  (list->vector (list
                 (task null navigate-to-hq)
                 (task null negotiate))))

(define (run-navigate-test)
  (define queue (make-queue))
  (define scripts (make-hash))
  
  (hash-set! scripts 'ship-1-script-id (build-navigate-script "REDSHIFT-1" "X1-NU19-72345Z"))
  (hash-set! scripts 'ship-2-script-id (build-navigate-script "REDSHIFT-2" "X1-NU19-72345Z"))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'ship-1-script-id 0) (make-state)))

  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'ship-2-script-id 0) (make-state)))

  (process-queue scripts queue))

(define (run-negotiate-test)
  (define queue (make-queue))
  (define scripts (make-hash))
  
  (hash-set! scripts 'negotiate (build-negotiate-script "REDSHIFT-1"))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'negotiate 0) (make-state)))

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
            
;; navigate the ship to the specified location
;; do nothing if the ship is already there
(define (navigate ship-symbol destination-symbol)
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  (orbit-ship ship-symbol)
  
  (printf "navigate ~s ~s~n" ship-symbol destination-symbol)
  (let ([current-location (ship-location ship-symbol)])
    (cond
      [(equal? current-location destination-symbol)
       ;; wait for a second to avoid triggering API limits
       (increment-current-utc-date 1)]
      [else
       (let ([nav-result (navigate-ship ship-symbol destination-symbol)])
         (nav-result-arrival nav-result))])))
      
;; assume ship is at headquarters
;; dock
;; negotiate-contract
(define (negotiate-contract ship-symbol)
  (dock-ship ship-symbol)
  (refuel-ship ship-symbol)  
  
  (printf "negotiate contract ~s~n" ship-symbol)
  (let ([result (ship-negotiate-contract ship-symbol)])
    (printf "contract result: ~n~s~n" result)))
