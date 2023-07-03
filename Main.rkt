#lang racket

;; 

(require space-traders-v2/access)
(require space-traders-v2/agents)
(require space-traders-v2/contracts)
(require space-traders-v2/fleet)
(require space-traders-v2/wrappers)

(require "lenses/agent.rkt")

(require "directory.rkt")
(require "try-scripts.rkt")
(require "wait-queue.rkt")

(define (read-access-token)
  (define access-token-path "access-token.txt")
  (string-trim (port->string (open-input-file access-token-path) #:close? #t)))

(define (run fn)
  (parameterize ([access-token (read-access-token)])
    (fn)))

(define (extract-loop)
  (define ship-id "DRFOGOUT-1")
  (define queue (make-queue))
  
  (printf "start extract loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent))))
  
  (define extract-script (build-extract-loop-script ship-id
                                                    asteroid-field-id
                                                    system-id
                                                    market-dest-id
                                                    asteroid-field-id))
  (define scripts (hash 'extract extract-script))
  (define state (hash 'count 0 'max-count 10))
  
  (queue-push-by-date! queue
                       (current-utc-date)
                       (task-step (script-pos 'extract 0) state))

  (process-queue scripts queue)
  
  (printf "finish extract loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent)))))
