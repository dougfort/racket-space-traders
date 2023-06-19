#lang racket

;; 

(provide search search-jump-gate)

(require "explore.rkt")
(require "api/agents.rkt")
(require "api/systems.rkt")
(require "api/wrappers.rkt")
(require "lenses/agent.rkt")

(define jump-gate "X1-KS52-51429E")
(define asteroid-field "X1-GX66-49714D")
(define possible-market "X1-GX66-49714D")

;; perform a breadth first seatch on a tree of jump gates
(define (search-jump-gate system-id waypoint-id)
  (let ([jump-gate-data (data (get-jump-gate system-id waypoint-id))])
    (printf "gate ~s; range ~s; faction~s~n"
            waypoint-id
            (hash-ref jump-gate-data 'jumpRange)
            (hash-ref jump-gate-data 'factionSymbol))
    (for ([system (in-list (hash-ref jump-gate-data 'connectedSystems))])
      (let ([system-id (hash-ref system 'symbol)])
        (printf "    ~s ~s ~s ~s~n"
                system-id
                (hash-ref system 'type)
                (hash-ref system 'factionSymbol)
                (hash-ref system 'distance))
        (display-waypoint-traits system-id)
        (println "*****")
        (display-all-markets system-id)))))                        

(define (search)
  ;; start with the agent hq
  (define hq (agent-headquarters (data (get-agent))))
  (printf "starting at agent hq: ~a" hq)

  ;; get the local waypoints
)