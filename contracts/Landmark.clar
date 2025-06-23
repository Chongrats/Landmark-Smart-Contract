(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_PROPERTY_EXISTS (err u102))
(define-constant ERR_NOT_OWNER (err u103))
(define-constant ERR_INVALID_PRICE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_PROPERTY_NOT_FOR_SALE (err u107))
(define-constant ERR_CANNOT_BUY_OWN_PROPERTY (err u108))

(define-data-var property-counter uint u0)

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    address: (string-ascii 200),
    coordinates: { lat: int, lng: int },
    size: uint,
    property-type: (string-ascii 50),
    registered-at: uint,
    last-transfer: uint,
    value: uint,
    for-sale: bool,
    sale-price: uint
  }
)

(define-map property-history
  { property-id: uint, transfer-id: uint }
  {
    from: principal,
    to: principal,
    price: uint,
    timestamp: uint,
    transfer-type: (string-ascii 20)
  }
)

(define-map transfer-counter
  { property-id: uint }
  { count: uint }
)

(define-map owner-properties
  { owner: principal }
  { property-ids: (list 100 uint) }
)

(define-public (register-property 
  (address (string-ascii 200))
  (lat int)
  (lng int)
  (size uint)
  (property-type (string-ascii 50))
  (initial-value uint))
  (let
    (
      (property-id (+ (var-get property-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> size u0) ERR_INVALID_PRICE)
    (asserts! (> initial-value u0) ERR_INVALID_PRICE)
    
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        address: address,
        coordinates: { lat: lat, lng: lng },
        size: size,
        property-type: property-type,
        registered-at: current-block,
        last-transfer: current-block,
        value: initial-value,
        for-sale: false,
        sale-price: u0
      }
    )
    
    (map-set property-history
      { property-id: property-id, transfer-id: u0 }
      {
        from: CONTRACT_OWNER,
        to: tx-sender,
        price: u0,
        timestamp: current-block,
        transfer-type: "registration"
      }
    )
    
    (map-set transfer-counter
      { property-id: property-id }
      { count: u1 }
    )
    
    (update-owner-properties tx-sender property-id)
    (var-set property-counter property-id)
    (ok property-id)
  )
)

(define-public (transfer-property (property-id uint) (new-owner principal))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (transfer-count (default-to { count: u0 } (map-get? transfer-counter { property-id: property-id })))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (not (is-eq tx-sender new-owner)) ERR_CANNOT_BUY_OWN_PROPERTY)
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        owner: new-owner,
        last-transfer: current-block,
        for-sale: false,
        sale-price: u0
      })
    )
    
    (map-set property-history
      { property-id: property-id, transfer-id: (get count transfer-count) }
      {
        from: tx-sender,
        to: new-owner,
        price: u0,
        timestamp: current-block,
        transfer-type: "transfer"
      }
    )
    
    (map-set transfer-counter
      { property-id: property-id }
      { count: (+ (get count transfer-count) u1) }
    )
    
    (remove-property-from-owner tx-sender property-id)
    (update-owner-properties new-owner property-id)
    (ok true)
  )
)

(define-public (list-for-sale (property-id uint) (price uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        for-sale: true,
        sale-price: price
      })
    )
    (ok true)
  )
)

(define-public (remove-from-sale (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        for-sale: false,
        sale-price: u0
      })
    )
    (ok true)
  )
)

(define-public (buy-property (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (transfer-count (default-to { count: u0 } (map-get? transfer-counter { property-id: property-id })))
      (sale-price (get sale-price property))
      (current-owner (get owner property))
    )
    (asserts! (get for-sale property) ERR_PROPERTY_NOT_FOR_SALE)
    (asserts! (not (is-eq tx-sender current-owner)) ERR_CANNOT_BUY_OWN_PROPERTY)
    (asserts! (>= (stx-get-balance tx-sender) sale-price) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? sale-price tx-sender current-owner))
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        owner: tx-sender,
        last-transfer: current-block,
        for-sale: false,
        sale-price: u0,
        value: sale-price
      })
    )
    
    (map-set property-history
      { property-id: property-id, transfer-id: (get count transfer-count) }
      {
        from: current-owner,
        to: tx-sender,
        price: sale-price,
        timestamp: current-block,
        transfer-type: "sale"
      }
    )
    
    (map-set transfer-counter
      { property-id: property-id }
      { count: (+ (get count transfer-count) u1) }
    )
    
    (remove-property-from-owner current-owner property-id)
    (update-owner-properties tx-sender property-id)
    (ok true)
  )
)

(define-public (update-property-value (property-id uint) (new-value uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (> new-value u0) ERR_INVALID_PRICE)
    
    (map-set properties
      { property-id: property-id }
      (merge property { value: new-value })
    )
    (ok true)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-property-owner (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (some (get owner property))
    none
  )
)

(define-read-only (get-property-history (property-id uint) (transfer-id uint))
  (map-get? property-history { property-id: property-id, transfer-id: transfer-id })
)

(define-read-only (get-transfer-count (property-id uint))
  (match (map-get? transfer-counter { property-id: property-id })
    counter (get count counter)
    u0
  )
)

(define-read-only (get-owner-properties (owner principal))
  (match (map-get? owner-properties { owner: owner })
    propertiess (get property-ids propertiess)
    (list)
  )
)

(define-read-only (get-total-properties)
  (var-get property-counter)
)

(define-read-only (is-property-for-sale (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (get for-sale property)
    false
  )
)

(define-read-only (get-property-coordinates (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (some (get coordinates property))
    none
  )
)

(define-private (update-owner-properties (owner principal) (property-id uint))
  (let
    (
      (current-properties (get property-ids (default-to { property-ids: (list) } (map-get? owner-properties { owner: owner }))))
    )
    (map-set owner-properties
      { owner: owner }
      { property-ids: (unwrap-panic (as-max-len? (append current-properties property-id) u100)) }
    )
  )
)

(define-private (remove-property-from-owner (owner principal) (property-id uint))
  (let
    (
      (current-properties (get property-ids (default-to { property-ids: (list) } (map-get? owner-properties { owner: owner }))))
      (filtered-properties (filter is-not-target-property current-properties))
    )
    (map-set owner-properties
      { owner: owner }
      { property-ids: filtered-properties }
    )
  )
)

(define-private (is-not-target-property (id uint))
  true
)