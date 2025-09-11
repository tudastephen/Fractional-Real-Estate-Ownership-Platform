;; Property Maintenance Escrow System
;; Automated allocation and management of property maintenance funds

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PROPERTY_NOT_FOUND (err u201))
(define-constant ERR_INSUFFICIENT_FUNDS (err u202))
(define-constant ERR_WORK_ORDER_NOT_FOUND (err u203))
(define-constant ERR_INVALID_BID (err u204))
(define-constant ERR_BID_EXPIRED (err u205))
(define-constant ERR_WORK_ORDER_COMPLETED (err u206))
(define-constant ERR_EMERGENCY_THRESHOLD_NOT_MET (err u207))
(define-constant ERR_INVALID_MAINTENANCE_RATE (err u208))
(define-constant ERR_NO_BIDS_AVAILABLE (err u209))

;; Data variables
(define-data-var next-work-order-id uint u1)
(define-data-var next-bid-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; Property maintenance fund tracking
(define-map property-maintenance-funds
    uint ;; property-id
    {
        total-allocated: uint,
        available-balance: uint,
        maintenance-rate: uint, ;; percentage of rental income (basis points)
        emergency-threshold: uint, ;; minimum balance for emergency access
        last-funding: uint
    }
)

;; Work orders for maintenance requests
(define-map maintenance-work-orders
    uint ;; work-order-id
    {
        property-id: uint,
        requester: principal,
        description: (string-ascii 500),
        estimated-cost: uint,
        priority: (string-ascii 20), ;; "LOW", "MEDIUM", "HIGH", "EMERGENCY"
        status: (string-ascii 20), ;; "OPEN", "IN_PROGRESS", "COMPLETED", "CANCELLED"
        approved-bid-id: (optional uint),
        completion-date: (optional uint),
        created-at: uint
    }
)

;; Contractor bids for work orders
(define-map contractor-bids
    uint ;; bid-id
    {
        work-order-id: uint,
        contractor: principal,
        bid-amount: uint,
        completion-timeframe: uint, ;; estimated days
        contractor-rating: uint, ;; out of 10000
        bid-expiry: uint,
        description: (string-ascii 300),
        accepted: bool
    }
)

;; Emergency fund releases tracking
(define-map emergency-releases
    {property-id: uint, release-id: uint}
    {
        amount: uint,
        reason: (string-ascii 200),
        approver: principal,
        release-date: uint,
        recipient: principal
    }
)

(define-data-var next-emergency-release-id uint u1)

;; Initialize maintenance fund for a property
(define-public (initialize-maintenance-fund (property-id uint) (maintenance-rate uint))
    (let ((property-check (contract-call? .fractionalEstate get-property property-id)))
        (asserts! (is-some property-check) ERR_PROPERTY_NOT_FOUND)
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (and (> maintenance-rate u0) (<= maintenance-rate u2000)) ERR_INVALID_MAINTENANCE_RATE) ;; max 20%
        
        (map-set property-maintenance-funds property-id
            {
                total-allocated: u0,
                available-balance: u0,
                maintenance-rate: maintenance-rate,
                emergency-threshold: u0,
                last-funding: stacks-block-height
            }
        )
        (ok property-id)
    )
)

;; Allocate funds from rental income to maintenance escrow
(define-public (allocate-maintenance-funds (property-id uint) (rental-amount uint))
    (let (
        (fund-data (unwrap! (map-get? property-maintenance-funds property-id) ERR_PROPERTY_NOT_FOUND))
        (maintenance-allocation (/ (* rental-amount (get maintenance-rate fund-data)) u10000))
        (new-threshold (/ (get available-balance fund-data) u10)) ;; 10% of current balance
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (> rental-amount u0) ERR_INSUFFICIENT_FUNDS)
        
        (map-set property-maintenance-funds property-id
            (merge fund-data
                {
                    total-allocated: (+ (get total-allocated fund-data) maintenance-allocation),
                    available-balance: (+ (get available-balance fund-data) maintenance-allocation),
                    emergency-threshold: (+ new-threshold u50000), ;; minimum 500 STX equivalent
                    last-funding: stacks-block-height
                }
            )
        )
        (ok maintenance-allocation)
    )
)

;; Create a maintenance work order
(define-public (create-work-order (property-id uint) (description (string-ascii 500)) (estimated-cost uint) (priority (string-ascii 20)))
    (let (
        (work-order-id (var-get next-work-order-id))
        (property-check (contract-call? .fractionalEstate get-property property-id))
        (user-shares (contract-call? .fractionalEstate get-user-shares property-id tx-sender))
    )
        (asserts! (is-some property-check) ERR_PROPERTY_NOT_FOUND)
        (asserts! (> (get shares user-shares) u0) ERR_UNAUTHORIZED) ;; must own shares to request maintenance
        
        (map-set maintenance-work-orders work-order-id
            {
                property-id: property-id,
                requester: tx-sender,
                description: description,
                estimated-cost: estimated-cost,
                priority: priority,
                status: "OPEN",
                approved-bid-id: none,
                completion-date: none,
                created-at: stacks-block-height
            }
        )
        
        (var-set next-work-order-id (+ work-order-id u1))
        (ok work-order-id)
    )
)

;; Submit contractor bid for work order
(define-public (submit-bid (work-order-id uint) (bid-amount uint) (timeframe uint) (description (string-ascii 300)))
    (let (
        (bid-id (var-get next-bid-id))
        (work-order (unwrap! (map-get? maintenance-work-orders work-order-id) ERR_WORK_ORDER_NOT_FOUND))
        (fund-data (unwrap! (map-get? property-maintenance-funds (get property-id work-order)) ERR_PROPERTY_NOT_FOUND))
    )
        (asserts! (is-eq (get status work-order) "OPEN") ERR_WORK_ORDER_COMPLETED)
        (asserts! (> bid-amount u0) ERR_INVALID_BID)
        (asserts! (<= bid-amount (get available-balance fund-data)) ERR_INSUFFICIENT_FUNDS)
        
        (map-set contractor-bids bid-id
            {
                work-order-id: work-order-id,
                contractor: tx-sender,
                bid-amount: bid-amount,
                completion-timeframe: timeframe,
                contractor-rating: u7500, ;; default rating
                bid-expiry: (+ stacks-block-height u1440), ;; 24 hour expiry
                description: description,
                accepted: false
            }
        )
        
        (var-set next-bid-id (+ bid-id u1))
        (ok bid-id)
    )
)

;; Accept a contractor bid
(define-public (accept-bid (bid-id uint))
    (let (
        (bid (unwrap! (map-get? contractor-bids bid-id) ERR_INVALID_BID))
        (work-order (unwrap! (map-get? maintenance-work-orders (get work-order-id bid)) ERR_WORK_ORDER_NOT_FOUND))
        (fund-data (unwrap! (map-get? property-maintenance-funds (get property-id work-order)) ERR_PROPERTY_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (< stacks-block-height (get bid-expiry bid)) ERR_BID_EXPIRED)
        (asserts! (is-eq (get status work-order) "OPEN") ERR_WORK_ORDER_COMPLETED)
        (asserts! (<= (get bid-amount bid) (get available-balance fund-data)) ERR_INSUFFICIENT_FUNDS)
        
        ;; Update work order status
        (map-set maintenance-work-orders (get work-order-id bid)
            (merge work-order 
                {
                    status: "IN_PROGRESS",
                    approved-bid-id: (some bid-id)
                }
            )
        )
        
        ;; Mark bid as accepted
        (map-set contractor-bids bid-id
            (merge bid {accepted: true})
        )
        
        ;; Reserve funds
        (map-set property-maintenance-funds (get property-id work-order)
            (merge fund-data
                {
                    available-balance: (- (get available-balance fund-data) (get bid-amount bid))
                }
            )
        )
        
        ;; Pay contractor
        (try! (stx-transfer? (get bid-amount bid) (as-contract tx-sender) (get contractor bid)))
        
        (ok bid-id)
    )
)

;; Complete work order
(define-public (complete-work-order (work-order-id uint))
    (let (
        (work-order (unwrap! (map-get? maintenance-work-orders work-order-id) ERR_WORK_ORDER_NOT_FOUND))
        (approved-bid-id (unwrap! (get approved-bid-id work-order) ERR_INVALID_BID))
        (bid (unwrap! (map-get? contractor-bids approved-bid-id) ERR_INVALID_BID))
    )
        (asserts! (or (is-eq tx-sender (get contractor bid)) (is-eq tx-sender (var-get contract-owner))) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status work-order) "IN_PROGRESS") ERR_WORK_ORDER_COMPLETED)
        
        (map-set maintenance-work-orders work-order-id
            (merge work-order
                {
                    status: "COMPLETED",
                    completion-date: (some stacks-block-height)
                }
            )
        )
        (ok work-order-id)
    )
)

;; Emergency fund release (requires owner approval)
(define-public (emergency-fund-release (property-id uint) (amount uint) (reason (string-ascii 200)) (recipient principal))
    (let (
        (fund-data (unwrap! (map-get? property-maintenance-funds property-id) ERR_PROPERTY_NOT_FOUND))
        (release-id (var-get next-emergency-release-id))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (>= amount (get emergency-threshold fund-data)) ERR_EMERGENCY_THRESHOLD_NOT_MET)
        (asserts! (<= amount (get available-balance fund-data)) ERR_INSUFFICIENT_FUNDS)
        
        ;; Record emergency release
        (map-set emergency-releases
            {property-id: property-id, release-id: release-id}
            {
                amount: amount,
                reason: reason,
                approver: tx-sender,
                release-date: stacks-block-height,
                recipient: recipient
            }
        )
        
        ;; Update fund balance
        (map-set property-maintenance-funds property-id
            (merge fund-data
                {
                    available-balance: (- (get available-balance fund-data) amount)
                }
            )
        )
        
        ;; Transfer funds
        (try! (stx-transfer? amount (as-contract tx-sender) recipient))
        
        (var-set next-emergency-release-id (+ release-id u1))
        (ok release-id)
    )
)

;; Read-only functions
(define-read-only (get-maintenance-fund (property-id uint))
    (map-get? property-maintenance-funds property-id)
)

(define-read-only (get-work-order (work-order-id uint))
    (map-get? maintenance-work-orders work-order-id)
)

(define-read-only (get-bid (bid-id uint))
    (map-get? contractor-bids bid-id)
)

(define-read-only (get-emergency-release (property-id uint) (release-id uint))
    (map-get? emergency-releases {property-id: property-id, release-id: release-id})
)

(define-read-only (get-fund-summary (property-id uint))
    (let ((fund-data (map-get? property-maintenance-funds property-id)))
        (match fund-data
            fund-info
            (ok {
                property-id: property-id,
                total-allocated: (get total-allocated fund-info),
                available-balance: (get available-balance fund-info),
                maintenance-rate: (get maintenance-rate fund-info),
                emergency-threshold: (get emergency-threshold fund-info),
                utilization-rate: (if (> (get total-allocated fund-info) u0)
                    (/ (* (- (get total-allocated fund-info) (get available-balance fund-info)) u10000) 
                       (get total-allocated fund-info))
                    u0)
            })
            (err ERR_PROPERTY_NOT_FOUND)
        )
    )
)
