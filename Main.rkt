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

(require "try-scripts.rkt")
(require "wait-queue.rkt")
(require "initial.rkt")
(require "runner.rkt")

(define (extract-loop)

  (let ([state (initial-scan (hash 'count 0 'max-count 10))])
    (printf "start extract loop: ship: ~s credits ~s~n"
            (hash-ref state 'command-ship-symbol)
            (agent-credits (data (get-agent))))
  
    (process-queue (hash 'extract (build-extract-loop-script))
                   state
                   (queue-push-by-date (make-queue)
                                       (current-utc-date)
                                       (script-pos 'extract 0)))
  
    (printf "finish extract loop: ship: ~s credits ~s~n"
            (hash-ref state 'command-ship-symbol)
            (agent-credits (data (get-agent))))))
