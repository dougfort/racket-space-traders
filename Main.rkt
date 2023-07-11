#lang racket

;; 

(require space-traders-v2/agents)
(require space-traders-v2/contracts)
(require space-traders-v2/fleet)
(require space-traders-v2/systems)
(require space-traders-v2/wrappers)

(require "lenses/agent.rkt")
(require "lenses/ship.rkt")
(require "lenses/waypoint.rkt")

(require "directory.rkt")
(require "try-scripts.rkt")
(require "wait-queue.rkt")
(require "runner.rkt")

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
                       (script-pos 'extract 0))

  (process-queue scripts state queue)
  
  (printf "finish extract loop: ship: ~s credits ~s~n" ship-id (agent-credits (data (get-agent)))))
