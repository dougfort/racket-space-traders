#lang racket

;; 

(require racket/date)
(require "api/agents.rkt")
(require "api/fleet.rkt")
(require "wait-queue.rkt")

(define (current-utc-date)
  (seconds->date (current-seconds) #f))

(define (make-state script-id)
  (hash 'script-id script-id 'script-index 0))

(struct task-result (timestamp state))

;; navigate to agent headquarters
;; then navige to the asteroid field and back
;; 3 times
(define (build-navigate-script)
  (define ship-id "REDSHIFT-1")
  (define asteroid-field "X1-NU19-72345Z")
  (define hq (agent-headquarters))
  (define max-count 3)
  
  (list->vector (list
                 (λ (state) (let* ([timestamp (navigate ship-id hq)]
                                   [next-index (add1 (hash-ref state 'script-index))]
                                   [next-state (hash-set state 'script-index next-index)])
                               (task-result timestamp next-state)))
                 (λ (state) (let ([timestamp (navigate ship-id asteroid-field)]
                                   [next-index (add1 (hash-ref state 'script-index))])
                               (task-result timestamp (hash-set state 'script-index next-index))))
                 (λ (state) (let ([timestamp (navigate ship-id hq)]
                                   [next-index (add1 (hash-ref state 'script-index))])
                               (task-result timestamp (hash-set state 'script-index next-index))))
                 (λ (state) (let* ([timestamp (current-utc-date)]
                                    [count (hash-ref state 'count 0)]
                                    [next-index (cond                                
                                                  [(>= count max-count)
                                                   (add1 (hash-ref state 'script-index))]
                                                  [else
                                                   (- (hash-ref state 'script-index) 2)])]
                                    [next-count (cond                                
                                                  [(>= count max-count) 0]
                                                  [else (add1 count)])])
                               (task-result timestamp (hash-set* state
                                                                 'script-index next-index
                                                                 'count next-count)))))))
                                  
                                      

(define (run)
  (define queue (make-queue))
  (define scripts (make-hash))
  (define navigate-script-id 'navigate-script)
  

  (hash-set! scripts navigate-script-id (build-navigate-script))
  
  (queue-push-by-date! queue (current-utc-date) (make-state navigate-script-id))

  (process-queue scripts queue))

(define (process-queue scripts queue)
  (cond
    [(queue-empty? queue) (println "queue is empty")]
    [else
     (queue-wait queue)
     (let* ([state (queue-pop queue)]
            [script-id (hash-ref state 'script-id)]
            [script-index (hash-ref state 'script-index)]
            [script (hash-ref scripts script-id)])
       (printf "script-id ~s; script-index ~s; count ~s~n"
               script-id script-index (hash-ref state 'count 0))
       (cond
         [(>= script-index (vector-length script))
          (printf "end of script ~s~n" script-id)]
         [else
          (let ([fn (vector-ref script script-index)])
            (let ([result (fn state)])
              (queue-push-by-date! queue
                                   (task-result-timestamp result)
                                   (task-result-state result))))])
          (process-queue scripts queue))]))

            
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
      
