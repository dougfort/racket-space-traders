#lang racket

;; 

;; This module implements the 'fleet' section of the Space Trader V2 API
;; https://docs.spacetraders.io/api-guide/open-api-spec

(provide list-ships
         purchase-ship
         get-ship
         get-ship-cargo
         orbit-ship
         ship-refine
         create-chart
         get-ship-cooldown
         dock-ship
         create-survey
         extract-resources
         jettison-cargo
         jump-ship
         navigate-ship
         patch-ship-nav
         get-ship-nav
         warp-ship
         sell-cargo
         scan-systems
         scan-waypoints
         scan-ships
         refuel-ship
         purchase-cargo
         transfer-cargo         
         negotiate-contract
         get-mounts
         install-mount
         remove-mount)

(require threading)
(require "http.rkt")

;; Retrieve all of your ships.
(define (list-ships)
  (hash-ref (api-get "/v2/my/ships") 'data))

;; Purchase a ship
(define (purchase-ship ship-type waypoint-symbol)
  (let ([uri "/v2/my/ships/"]
        [data (hash 'shipType ship-type 'waypointSymbol waypoint-symbol)])
    (hash-ref (api-post uri data 201) 'data)))

;; Retrieve the details of your ship.
(define (get-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol) "")])
    (hash-ref (api-get uri) 'data)))

;; Retrieve the cargo of your ship.
(define (get-ship-cargo ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/cargo") "")])
    (hash-ref (api-get uri) 'data)))

;; Attempt to move your ship into orbit at it's current location.
;;
;; The request will only succeed if your ship is capable of moving into orbit
;; at the time of the request.
;;
;; The endpoint is idempotent - successive calls will succeed even if the ship is already in orbit.
(define (orbit-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/orbit") "")])
    (hash-ref (api-post uri #f) 'data)))

;; Attempt to refine the raw materials on your ship.
;; The request will only succeed if your ship is capable of refining at the time of the request.
(define (ship-refine ship-symbol material)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/refine") "")]
        [data (hash 'produce material)])
    (hash-ref (api-post uri data) 'data)))

;; Command a ship to chart the current waypoint.
;;
;; Waypoints in the universe are uncharted by default.
;; These locations will not show up in the API until they have been charted by a ship.
;;
;; Charting a location will record your agent as the one who created the chart
(define (create-chart ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/chart") "")])
    (hash-ref (api-post uri #f 201) 'data)))

;; Retrieve the details of your ship's reactor cooldown.
;; Some actions such as activating your jump drive, scanning,
;; or extracting resources taxes your reactor and results in a cooldown.
;;
;; Your ship cannot perform additional actions until your cooldown has expired.
;; The duration of your cooldown is relative to the power consumption of the related modules
;; or mounts for the action taken.
;;
;; Response returns a 204 status code (no-content) when the ship has no cooldown.
(define (get-ship-cooldown ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/cooldown") "")])
    (hash-ref (api-get uri '(200 204)) 'data)))

;; Attempt to dock your ship at it's current location.
;; Docking will only succeed if the waypoint is a dockable location,
;; and your ship is capable of docking at the time of the request.
;;
;; The endpoint is idempotent - successive calls will succeed even if the ship is already docked.
(define (dock-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/dock") "")])
    (hash-ref (api-post uri #f) 'data)))

;; If you want to target specific yields for an extraction, you can survey a waypoint,
;; such as an asteroid field, and send the survey in the body of the extract request.
;; Each survey may have multiple deposits, and if a symbol shows up more than once,
;; that indicates a higher chance of extracting that resource.
;;
;; Your ship will enter a cooldown between consecutive survey requests.
;; Surveys will eventually expire after a period of time.
;; Multiple ships can use the same survey for extraction.
(define (create-survey ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/survey") "")])
    (hash-ref (api-post uri #f '(201)) 'data)))

;; Extract resources from the waypoint into your ship.
;; Send an optional survey as the payload to target specific yields.
(define (extract-resources ship-symbol [survey #f])
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/extract") "")]
        [data (if survey (hash 'survey survey) #f)])
    (hash-ref (api-post uri data '(201)) 'data)))

;; Jettison cargo from your ship's cargo hold.
(define (jettison-cargo ship-symbol cargo-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/jettison") "")]
        [data (hash 'symbol cargo-symbol 'units units)])
    (hash-ref (api-post uri data) 'data)))

;; Jump your ship instantly to a target system.
;; When used while in orbit or docked to a jump gate waypoint, any ship can use this command.
;; When used elsewhere, jumping requires a jump drive unit and consumes a unit of antimatter
;; (which needs to be in your cargo).
(define (jump-ship ship-symbol system-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/jump") "")]
        [data (hash 'systemSymbol system-symbol)])
    (hash-ref (api-post uri data) 'data)))

;; Navigate to a target destination.
;; The destination must be located within the same system as the ship.
;; Navigating will consume the necessary fuel and supplies from the ship's manifest,
;; and will pay out crew wages from the agent's account.
;;
;; The returned response will detail the route information including the expected time of arrival.
;; Most ship actions are unavailable until the ship has arrived at it's destination.
;;
;; To travel between systems, see the ship's warp or jump actions.
(define (navigate-ship ship-symbol waypoint-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/navigate") "")]
        [data (hash 'waypointSymbol waypoint-symbol)])
    (hash-ref (api-post uri data) 'data)))

;; Update the nav data of a ship, such as the flight mode.
(define (patch-ship-nav ship-symbol flight-mode)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/nav") "")]
        [data (hash 'flightMode flight-mode)])
    (hash-ref (api-patch uri data) 'data)))

;; Get the current nav status of a ship.
(define (get-ship-nav ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/nav") "")])
    (hash-ref (api-get uri) 'data)))

;; Warp your ship to a target destination in another system.
;; Warping will consume the necessary fuel and supplies from the ship's manifest,
;; and will pay out crew wages from the agent's account.
;;
;; The returned response will detail the route information including the expected time of arrival.
;; Most ship actions are unavailable until the ship has arrived at it's destination.
(define (warp-ship ship-symbol waypoint-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/warp") "")]
        [data (hash 'waypointSymbol waypoint-symbol)])
    (hash-ref (api-post uri data) 'data)))

;; Sell cargo.
(define (sell-cargo ship-symbol cargo-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/sell") "")]
        [data (hash 'symbol cargo-symbol 'units units)])
    (hash-ref (api-post uri data '(201)) 'data)))

;; Activate your ship's sensor arrays to scan for system information.
(define (scan-systems ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/scan/systems") "")])
    (hash-ref (api-post uri #f '(201)) 'data)))

;; Activate your ship's sensor arrays to scan for system information.
(define (scan-waypoints ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/scan/waypoints") "")])
    (hash-ref (api-post uri #f '(201)) 'data)))

;; Activate your ship's sensor arrays to scan for ship information.
(define (scan-ships ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/scan/ships") "")])
    (hash-ref (api-post uri #f '(201)) 'data)))

;; Refuel your ship from the local market.
(define (refuel-ship ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/refuel") "")])
    (hash-ref (api-post uri #f) 'data)))

;; Purchase cargo.
(define (purchase-cargo ship-symbol cargo-symbol units)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/purchase") "")]
        [data (hash 'symbol cargo-symbol 'units units)])
    (hash-ref (api-post uri data) 'data)))

;; Transfer cargo between ships.
(define (transfer-cargo ship-symbol cargo-symbol units dest-ship)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/purchase") "")]
        [data (hash 'symbol cargo-symbol 'units units 'shipSymbol dest-ship)])
    (hash-ref (api-post uri data) 'data)))

;; Negotiate a contract
(define (negotiate-contract ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/negotiate/contract") "")])
    (hash-ref (api-post uri #f '(201)) 'data)))

;; Get the mounts on a ship.
(define (get-mounts ship-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/mounts") "")])
    (hash-ref (api-get uri) 'data)))

;; Install a mount on a ship.
(define (install-mount ship-symbol mount-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/mounts/install") "")]
        [data (hash 'symbol mount-symbol)])
    (hash-ref (api-post uri data '(201)) 'data)))

;; Remove a mount from a ship.
(define (remove-mount ship-symbol mount-symbol)
  (let ([uri (string-join (list "/v2/my/ships/" ship-symbol "/mounts/remove") "")]
        [data (hash 'symbol mount-symbol)])
    (hash-ref (api-post uri data '(201)) 'data)))
