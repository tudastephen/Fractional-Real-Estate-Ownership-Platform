
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

(define-data-var next-property-id uint u1)

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