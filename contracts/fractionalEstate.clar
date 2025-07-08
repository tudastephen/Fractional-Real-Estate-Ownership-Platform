
;; title: fractionalEstate

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-property-exists (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-property-not-active (err u105))
(define-constant err-insufficient-shares (err u106))
(define-constant err-property-not-found (err u107))
(define-constant err-shares-not-available (err u108))
(define-constant err-invalid-amount (err u109))

(define-constant err-rental-not-found (err u110))
(define-constant err-no-rental-income (err u111))

(define-data-var next-property-id uint u1)


(define-constant err-proposal-not-found (err u112))
(define-constant err-proposal-expired (err u113))
(define-constant err-proposal-not-active (err u114))
(define-constant err-already-voted (err u115))
(define-constant err-proposal-not-passed (err u116))
(define-constant err-proposal-already-executed (err u117))
(define-constant err-invalid-voting-period (err u118))
(define-constant err-invalid-threshold (err u119))
(define-constant err-no-shares (err u120))

(define-data-var next-proposal-id uint u1)

(define-map property-proposals
  { proposal-id: uint }
  {
    property-id: uint,
    proposer: principal,
    proposal-type: (string-ascii 50),
    description: (string-ascii 500),
    amount: uint,
    voting-deadline: uint,
    total-votes-for: uint,
    total-votes-against: uint,
    executed: bool,
    active: bool,
    execution-threshold: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)


(define-map properties
  { property-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    total-shares: uint,
    available-shares: uint,
    price-per-share: uint,
    active: bool,
    owner: principal
  }
)

(define-map property-shares
  { property-id: uint, owner: principal }
  { shares: uint }
)

(define-map property-dividends
  { property-id: uint }
  { total-dividends: uint, dividends-per-share: uint }
)

(define-map user-dividends
  { property-id: uint, user: principal }
  { claimed-dividends-per-share: uint }
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-user-shares (property-id uint) (user principal))
  (default-to { shares: u0 }
    (map-get? property-shares { property-id: property-id, owner: user })
  )
)

(define-read-only (get-property-dividends (property-id uint))
  (default-to { total-dividends: u0, dividends-per-share: u0 }
    (map-get? property-dividends { property-id: property-id })
  )
)

(define-read-only (get-claimable-dividends (property-id uint) (user principal))
  (let (
    (user-shares (get-user-shares property-id tx-sender))
    (property-div (get-property-dividends property-id))
    (user-div (default-to { claimed-dividends-per-share: u0 }
      (map-get? user-dividends { property-id: property-id, user: user })))
  )
  (if (> (get shares user-shares) u0)
    (let (
      (unclaimed-div-per-share (- (get dividends-per-share property-div) (get claimed-dividends-per-share user-div)))
      (total-unclaimed (* unclaimed-div-per-share (get shares user-shares)))
    )
    (ok total-unclaimed))
    (ok u0)
  ))
)

(define-public (register-property (name (string-ascii 100)) (location (string-ascii 100)) (total-shares uint) (price-per-share uint))
  (let ((property-id (var-get next-property-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-shares u0) err-invalid-amount)
    (asserts! (> price-per-share u0) err-invalid-amount)
    
    (map-set properties
      { property-id: property-id }
      {
        name: name,
        location: location,
        total-shares: total-shares,
        available-shares: total-shares,
        price-per-share: price-per-share,
        active: true,
        owner: contract-owner
      }
    )
    
    (map-set property-dividends
      { property-id: property-id }
      { total-dividends: u0, dividends-per-share: u0 }
    )
    
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (buy-shares (property-id uint) (shares uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (total-cost (* shares (get price-per-share property)))
    (user-current-shares (get shares (get-user-shares property-id tx-sender)))
  )
    (asserts! (get active property) err-property-not-active)
    (asserts! (<= shares (get available-shares property)) err-shares-not-available)
    (asserts! (> shares u0) err-invalid-amount)
    
    (try! (stx-transfer? total-cost tx-sender contract-owner))
    
    (map-set properties
      { property-id: property-id }
      (merge property { available-shares: (- (get available-shares property) shares) })
    )
    
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: (+ user-current-shares shares) }
    )
    
    (ok shares)
  )
)

(define-public (sell-shares (property-id uint) (shares uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (user-shares (get-user-shares property-id tx-sender))
    (refund-amount (* shares (get price-per-share property)))
  )
    (asserts! (get active property) err-property-not-active)
    (asserts! (>= (get shares user-shares) shares) err-insufficient-shares)
    (asserts! (> shares u0) err-invalid-amount)
    
    (try! (stx-transfer? refund-amount contract-owner tx-sender))
    
    (map-set properties
      { property-id: property-id }
      (merge property { available-shares: (+ (get available-shares property) shares) })
    )
    
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: (- (get shares user-shares) shares) }
    )
    
    (ok shares)
  )
)

(define-public (distribute-dividends (property-id uint) (amount uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (current-dividends (get-property-dividends property-id))
    (total-shares (get total-shares property))
    (new-dividends-per-share (/ amount total-shares))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> total-shares u0) err-invalid-amount)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set property-dividends
      { property-id: property-id }
      { 
        total-dividends: (+ (get total-dividends current-dividends) amount),
        dividends-per-share: (+ (get dividends-per-share current-dividends) new-dividends-per-share)
      }
    )
    
    (ok new-dividends-per-share)
  )
)

(define-public (claim-dividends (property-id uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (claimable-amount (unwrap! (get-claimable-dividends property-id tx-sender) err-not-found))
    (user-shares (get-user-shares property-id tx-sender))
    (property-div (get-property-dividends property-id))
  )
    (asserts! (> claimable-amount u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? claimable-amount tx-sender tx-sender)))
    
    (map-set user-dividends
      { property-id: property-id, user: tx-sender }
      { claimed-dividends-per-share: (get dividends-per-share property-div) }
    )
    
    (ok claimable-amount)
  )
)

(define-public (toggle-property-status (property-id uint))
  (let ((property (unwrap! (get-property property-id) err-property-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set properties
      { property-id: property-id }
      (merge property { active: (not (get active property)) })
    )
    
    (ok (not (get active property)))
  )
)



(define-public (transfer-property-ownership (property-id uint) (new-owner principal))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq new-owner contract-owner)) err-unauthorized)
    
    (map-set properties
      { property-id: property-id }
      (merge property { owner: new-owner })
    )
    
    (ok new-owner)
  )
)


(define-map share-trades 
  { trade-id: uint }
  {
    seller: principal,
    property-id: uint,
    shares: uint,
    price-per-share: uint,
    active: bool
  }
)

(define-data-var next-trade-id uint u1)

(define-public (create-share-trade (property-id uint) (shares uint) (price-per-share uint))
  (let (
    (trade-id (var-get next-trade-id))
    (user-shares (get-user-shares property-id tx-sender))
  )
    (asserts! (>= (get shares user-shares) shares) err-insufficient-shares)
    (asserts! (> shares u0) err-invalid-amount)
    (asserts! (> price-per-share u0) err-invalid-amount)
    
    (map-set share-trades
      { trade-id: trade-id }
      {
        seller: tx-sender,
        property-id: property-id,
        shares: shares,
        price-per-share: price-per-share,
        active: true
      }
    )
    
    (var-set next-trade-id (+ trade-id u1))
    (ok trade-id)
  )
)

(define-public (execute-share-trade (trade-id uint))
  (let (
    (trade (unwrap! (map-get? share-trades { trade-id: trade-id }) err-not-found))
    (total-cost (* (get shares trade) (get price-per-share trade)))
    (seller-shares (get-user-shares (get property-id trade) (get seller trade)))
    (buyer-shares (get-user-shares (get property-id trade) tx-sender))
  )
    (asserts! (get active trade) err-property-not-active)
    (asserts! (not (is-eq tx-sender (get seller trade))) err-unauthorized)
    
    (try! (stx-transfer? total-cost tx-sender (get seller trade)))
    
    (map-set property-shares
      { property-id: (get property-id trade), owner: (get seller trade) }
      { shares: (- (get shares seller-shares) (get shares trade)) }
    )
    
    (map-set property-shares
      { property-id: (get property-id trade), owner: tx-sender }
      { shares: (+ (get shares buyer-shares) (get shares trade)) }
    )
    
    (map-set share-trades
      { trade-id: trade-id }
      (merge trade { active: false })
    )
    
    (ok trade-id)
  )
)


(define-map property-rental-income
  { property-id: uint }
  { 
    total-rental-collected: uint,
    rental-income-per-share: uint,
    last-rental-date: uint
  }
)

(define-map user-rental-claims
  { property-id: uint, user: principal }
  { claimed-rental-per-share: uint }
)

(define-read-only (get-property-rental-income (property-id uint))
  (default-to 
    { total-rental-collected: u0, rental-income-per-share: u0, last-rental-date: u0 }
    (map-get? property-rental-income { property-id: property-id })
  )
)

(define-read-only (get-claimable-rental-income (property-id uint) (user principal))
  (let (
    (user-shares (get-user-shares property-id user))
    (rental-info (get-property-rental-income property-id))
    (user-claims (default-to 
      { claimed-rental-per-share: u0 }
      (map-get? user-rental-claims { property-id: property-id, user: user })
    ))
  )
    (if (> (get shares user-shares) u0)
      (let (
        (unclaimed-rental-per-share (- (get rental-income-per-share rental-info) (get claimed-rental-per-share user-claims)))
        (total-claimable (* unclaimed-rental-per-share (get shares user-shares)))
      )
        (ok total-claimable)
      )
      (ok u0)
    )
  )
)

(define-public (record-rental-income (property-id uint) (rental-amount uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (current-rental (get-property-rental-income property-id))
    (total-shares (get total-shares property))
    (rental-per-share (/ rental-amount total-shares))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> rental-amount u0) err-invalid-amount)
    (asserts! (> total-shares u0) err-invalid-amount)
    
    (try! (stx-transfer? rental-amount tx-sender (as-contract tx-sender)))
    
    (map-set property-rental-income
      { property-id: property-id }
      {
        total-rental-collected: (+ (get total-rental-collected current-rental) rental-amount),
        rental-income-per-share: (+ (get rental-income-per-share current-rental) rental-per-share),
        last-rental-date: stacks-block-height
      }
    )
    
    (ok rental-per-share)
  )
)

(define-public (claim-rental-income (property-id uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (claimable-amount (unwrap! (get-claimable-rental-income property-id tx-sender) err-rental-not-found))
    (rental-info (get-property-rental-income property-id))
  )
    (asserts! (> claimable-amount u0) err-no-rental-income)
    
    (try! (as-contract (stx-transfer? claimable-amount tx-sender tx-sender)))
    
    (map-set user-rental-claims
      { property-id: property-id, user: tx-sender }
      { claimed-rental-per-share: (get rental-income-per-share rental-info) }
    )
    
    (ok claimable-amount)
  )
)

(define-read-only (get-property-rental-yield (property-id uint))
  (let (
    (property (unwrap! (get-property property-id) err-property-not-found))
    (rental-info (get-property-rental-income property-id))
    (property-value (* (get total-shares property) (get price-per-share property)))
  )
    (if (> property-value u0)
      (ok (/ (* (get total-rental-collected rental-info) u10000) property-value))
      (ok u0)
    )
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? property-proposals { proposal-id: proposal-id })
)

(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-status (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (property (unwrap! (get-property (get property-id proposal)) err-property-not-found))
    (total-shares (get total-shares property))
    (votes-for (get total-votes-for proposal))
    (votes-against (get total-votes-against proposal))
    (total-votes (+ votes-for votes-against))
    (approval-rate (if (> total-votes u0) (/ (* votes-for u10000) total-votes) u0))
    (participation-rate (if (> total-shares u0) (/ (* total-votes u10000) total-shares) u0))
    (required-threshold (get execution-threshold proposal))
  )
    (ok {
      proposal-id: proposal-id,
      total-votes: total-votes,
      votes-for: votes-for,
      votes-against: votes-against,
      approval-rate: approval-rate,
      participation-rate: participation-rate,
      required-threshold: required-threshold,
      passed: (>= approval-rate required-threshold),
      expired: (> stacks-block-height (get voting-deadline proposal))
    })
  )
)

(define-public (create-proposal (property-id uint) (proposal-type (string-ascii 50)) (description (string-ascii 500)) (amount uint) (voting-period uint) (threshold uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (property (unwrap! (get-property property-id) err-property-not-found))
    (user-shares (get-user-shares property-id tx-sender))
    (voting-deadline (+ stacks-block-height voting-period))
  )
    (asserts! (> (get shares user-shares) u0) err-no-shares)
    (asserts! (> voting-period u0) err-invalid-voting-period)
    (asserts! (and (>= threshold u5000) (<= threshold u10000)) err-invalid-threshold)
    (asserts! (get active property) err-property-not-active)
    
    (map-set property-proposals
      { proposal-id: proposal-id }
      {
        property-id: property-id,
        proposer: tx-sender,
        proposal-type: proposal-type,
        description: description,
        amount: amount,
        voting-deadline: voting-deadline,
        total-votes-for: u0,
        total-votes-against: u0,
        executed: false,
        active: true,
        execution-threshold: threshold
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (property (unwrap! (get-property (get property-id proposal)) err-property-not-found))
    (user-shares (get-user-shares (get property-id proposal) tx-sender))
    (voting-power (get shares user-shares))
    (existing-vote (get-user-vote proposal-id tx-sender))
  )
    (asserts! (get active proposal) err-proposal-not-active)
    (asserts! (<= stacks-block-height (get voting-deadline proposal)) err-proposal-expired)
    (asserts! (> voting-power u0) err-no-shares)
    (asserts! (is-none existing-vote) err-already-voted)
    
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, voting-power: voting-power }
    )
    
    (if vote-for
      (map-set property-proposals
        { proposal-id: proposal-id }
        (merge proposal { total-votes-for: (+ (get total-votes-for proposal) voting-power) })
      )
      (map-set property-proposals
        { proposal-id: proposal-id }
        (merge proposal { total-votes-against: (+ (get total-votes-against proposal) voting-power) })
      )
    )
    
    (ok voting-power)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (property (unwrap! (get-property (get property-id proposal)) err-property-not-found))
    (total-shares (get total-shares property))
    (votes-for (get total-votes-for proposal))
    (votes-against (get total-votes-against proposal))
    (total-votes (+ votes-for votes-against))
    (approval-rate (if (> total-votes u0) (/ (* votes-for u10000) total-votes) u0))
    (proposal-type (get proposal-type proposal))
    (proposal-amount (get amount proposal))
  )
    (asserts! (get active proposal) err-proposal-not-active)
    (asserts! (> stacks-block-height (get voting-deadline proposal)) err-proposal-expired)
    (asserts! (not (get executed proposal)) err-proposal-already-executed)
    (asserts! (>= approval-rate (get execution-threshold proposal)) err-proposal-not-passed)
    
    (map-set property-proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, active: false })
    )
    
    (if (is-eq proposal-type "maintenance")
      (begin
        (try! (as-contract (stx-transfer? proposal-amount tx-sender (get proposer proposal))))
        (ok "maintenance-approved")
      )
      (if (is-eq proposal-type "renovation")
        (begin
          (try! (as-contract (stx-transfer? proposal-amount tx-sender (get proposer proposal))))
          (ok "renovation-approved")
        )
        (if (is-eq proposal-type "management-change")
          (begin
            (map-set properties
              { property-id: (get property-id proposal) }
              (merge property { owner: (get proposer proposal) })
            )
            (ok "management-changed")
          )
          (if (is-eq proposal-type "property-sale")
            (begin
              (map-set properties
                { property-id: (get property-id proposal) }
                (merge property { active: false })
              )
              (ok "property-sale-approved")
            )
            (ok "proposal-executed")
          )
        )
      )
    )
  )
)

(define-public (cancel-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
  )
    (asserts! (is-eq tx-sender (get proposer proposal)) err-unauthorized)
    (asserts! (get active proposal) err-proposal-not-active)
    (asserts! (not (get executed proposal)) err-proposal-already-executed)
    
    (map-set property-proposals
      { proposal-id: proposal-id }
      (merge proposal { active: false })
    )
    
    (ok proposal-id)
  )
)