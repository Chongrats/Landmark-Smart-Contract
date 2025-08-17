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
(define-constant ERR_PROPERTY_NOT_FOR_RENT (err u109))
(define-constant ERR_RENTAL_ALREADY_EXISTS (err u110))
(define-constant ERR_RENTAL_NOT_FOUND (err u111))
(define-constant ERR_RENT_OVERDUE (err u112))
(define-constant ERR_INVALID_RENTAL_TERMS (err u113))
(define-constant ERR_RENTAL_ACTIVE (err u114))
(define-constant ERR_NOT_TENANT (err u115))
(define-constant ERR_RENT_ALREADY_PAID (err u116))
(define-constant ERR_AUCTION_NOT_FOUND (err u117))
(define-constant ERR_AUCTION_ALREADY_EXISTS (err u118))
(define-constant ERR_AUCTION_NOT_ACTIVE (err u119))
(define-constant ERR_AUCTION_ENDED (err u120))
(define-constant ERR_BID_TOO_LOW (err u121))
(define-constant ERR_CANNOT_BID_OWN_AUCTION (err u122))
(define-constant ERR_AUCTION_NOT_ENDED (err u123))
(define-constant ERR_NO_BIDS_PLACED (err u124))
(define-constant ERR_RESERVE_NOT_MET (err u125))

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
    sale-price: uint,
    for-rent: bool,
    rental-price: uint,
    for-auction: bool
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

(define-map rental-agreements
  { property-id: uint }
  {
    tenant: principal,
    monthly-rent: uint,
    deposit: uint,
    start-date: uint,
    end-date: uint,
    last-payment: uint,
    next-payment-due: uint,
    rent-payments-made: uint,
    deposit-paid: bool,
    active: bool
  }
)

(define-map rental-payments
  { property-id: uint, payment-id: uint }
  {
    tenant: principal,
    amount: uint,
    payment-date: uint,
    payment-type: (string-ascii 20),
    late-fee: uint
  }
)

(define-map rental-payment-counter
  { property-id: uint }
  { count: uint }
)

(define-map property-auctions
  { property-id: uint }
  {
    seller: principal,
    starting-bid: uint,
    reserve-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    start-time: uint,
    end-time: uint,
    active: bool,
    bid-count: uint
  }
)

(define-map auction-bids
  { property-id: uint, bid-id: uint }
  {
    bidder: principal,
    amount: uint,
    timestamp: uint,
    refunded: bool
  }
)

(define-map bidder-escrow
  { property-id: uint, bidder: principal }
  { amount: uint }
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
        sale-price: u0,
        for-rent: false,
        rental-price: u0,
        for-auction: false
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
        sale-price: u0,
        for-rent: false,
        rental-price: u0,
        for-auction: false
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
        value: sale-price,
        for-rent: false,
        rental-price: u0,
        for-auction: false
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

(define-public (list-for-rent (property-id uint) (monthly-rent uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (> monthly-rent u0) ERR_INVALID_PRICE)
    (asserts! (is-none (map-get? rental-agreements { property-id: property-id })) ERR_RENTAL_ALREADY_EXISTS)
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        for-rent: true,
        rental-price: monthly-rent
      })
    )
    (ok true)
  )
)

(define-public (remove-from-rent (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (rental (map-get? rental-agreements { property-id: property-id }))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (or (is-none rental) (not (get active (unwrap-panic rental)))) ERR_RENTAL_ACTIVE)
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        for-rent: false,
        rental-price: u0
      })
    )
    (ok true)
  )
)

(define-public (create-rental-agreement (property-id uint) (tenant principal) (deposit uint) (lease-months uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (monthly-rent (get rental-price property))
      (end-date (+ current-block (* lease-months u144)))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (get for-rent property) ERR_PROPERTY_NOT_FOR_RENT)
    (asserts! (> deposit u0) ERR_INVALID_RENTAL_TERMS)
    (asserts! (> lease-months u0) ERR_INVALID_RENTAL_TERMS)
    (asserts! (is-none (map-get? rental-agreements { property-id: property-id })) ERR_RENTAL_ALREADY_EXISTS)
    
    (map-set rental-agreements
      { property-id: property-id }
      {
        tenant: tenant,
        monthly-rent: monthly-rent,
        deposit: deposit,
        start-date: current-block,
        end-date: end-date,
        last-payment: u0,
        next-payment-due: (+ current-block u144),
        rent-payments-made: u0,
        deposit-paid: false,
        active: true
      }
    )
    
    (map-set rental-payment-counter
      { property-id: property-id }
      { count: u0 }
    )
    
    (map-set properties
      { property-id: property-id }
      (merge property { for-rent: false })
    )
    
    (ok true)
  )
)

(define-public (pay-deposit (property-id uint))
  (let
    (
      (rental (unwrap! (map-get? rental-agreements { property-id: property-id }) ERR_RENTAL_NOT_FOUND))
      (current-block stacks-block-height)
      (deposit-amount (get deposit rental))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (owner (get owner property))
    )
    (asserts! (is-eq (get tenant rental) tx-sender) ERR_NOT_TENANT)
    (asserts! (get active rental) ERR_RENTAL_NOT_FOUND)
    (asserts! (not (get deposit-paid rental)) ERR_RENT_ALREADY_PAID)
    (asserts! (>= (stx-get-balance tx-sender) deposit-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? deposit-amount tx-sender owner))
    
    (map-set rental-agreements
      { property-id: property-id }
      (merge rental { deposit-paid: true })
    )
    
    (let
      (
        (payment-count (get count (default-to { count: u0 } (map-get? rental-payment-counter { property-id: property-id }))))
      )
      (map-set rental-payments
        { property-id: property-id, payment-id: payment-count }
        {
          tenant: tx-sender,
          amount: deposit-amount,
          payment-date: current-block,
          payment-type: "deposit",
          late-fee: u0
        }
      )
      
      (map-set rental-payment-counter
        { property-id: property-id }
        { count: (+ payment-count u1) }
      )
    )
    
    (ok true)
  )
)

(define-public (pay-rent (property-id uint))
  (let
    (
      (rental (unwrap! (map-get? rental-agreements { property-id: property-id }) ERR_RENTAL_NOT_FOUND))
      (current-block stacks-block-height)
      (monthly-rent (get monthly-rent rental))
      (next-due (get next-payment-due rental))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (owner (get owner property))
      (late-fee (if (> current-block next-due) (/ monthly-rent u20) u0))
      (total-amount (+ monthly-rent late-fee))
    )
    (asserts! (is-eq (get tenant rental) tx-sender) ERR_NOT_TENANT)
    (asserts! (get active rental) ERR_RENTAL_NOT_FOUND)
    (asserts! (get deposit-paid rental) ERR_INVALID_RENTAL_TERMS)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? total-amount tx-sender owner))
    
    (map-set rental-agreements
      { property-id: property-id }
      (merge rental {
        last-payment: current-block,
        next-payment-due: (+ current-block u144),
        rent-payments-made: (+ (get rent-payments-made rental) u1)
      })
    )
    
    (let
      (
        (payment-count (get count (default-to { count: u0 } (map-get? rental-payment-counter { property-id: property-id }))))
      )
      (map-set rental-payments
        { property-id: property-id, payment-id: payment-count }
        {
          tenant: tx-sender,
          amount: total-amount,
          payment-date: current-block,
          payment-type: "rent",
          late-fee: late-fee
        }
      )
      
      (map-set rental-payment-counter
        { property-id: property-id }
        { count: (+ payment-count u1) }
      )
    )
    
    (ok true)
  )
)

(define-public (terminate-rental (property-id uint))
  (let
    (
      (rental (unwrap! (map-get? rental-agreements { property-id: property-id }) ERR_RENTAL_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (get active rental) ERR_RENTAL_NOT_FOUND)
    
    (map-set rental-agreements
      { property-id: property-id }
      (merge rental { active: false })
    )
    
    (map-set properties
      { property-id: property-id }
      (merge property {
        for-rent: true,
        rental-price: (get monthly-rent rental)
      })
    )
    
    (ok true)
  )
)

(define-public (create-auction (property-id uint) (starting-bid uint) (reserve-price uint) (duration-blocks uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (end-time (+ current-block duration-blocks))
    )
    (asserts! (is-eq (get owner property) tx-sender) ERR_NOT_OWNER)
    (asserts! (> starting-bid u0) ERR_INVALID_PRICE)
    (asserts! (>= reserve-price starting-bid) ERR_INVALID_PRICE)
    (asserts! (> duration-blocks u0) ERR_INVALID_PRICE)
    (asserts! (is-none (map-get? property-auctions { property-id: property-id })) ERR_AUCTION_ALREADY_EXISTS)
    (asserts! (not (get for-sale property)) ERR_PROPERTY_NOT_FOUND)
    (asserts! (not (get for-rent property)) ERR_PROPERTY_NOT_FOUND)
    
    (map-set property-auctions
      { property-id: property-id }
      {
        seller: tx-sender,
        starting-bid: starting-bid,
        reserve-price: reserve-price,
        current-bid: u0,
        highest-bidder: none,
        start-time: current-block,
        end-time: end-time,
        active: true,
        bid-count: u0
      }
    )
    
    (map-set properties
      { property-id: property-id }
      (merge property { for-auction: true })
    )
    
    (ok true)
  )
)

(define-public (place-bid (property-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? property-auctions { property-id: property-id }) ERR_AUCTION_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (current-bid (get current-bid auction))
      (minimum-bid (if (is-eq current-bid u0) (get starting-bid auction) (+ current-bid u1)))
      (current-escrow (get amount (default-to { amount: u0 } (map-get? bidder-escrow { property-id: property-id, bidder: tx-sender }))))
      (total-escrow (+ current-escrow bid-amount))
    )
    (asserts! (get active auction) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (<= current-block (get end-time auction)) ERR_AUCTION_ENDED)
    (asserts! (not (is-eq (get seller auction) tx-sender)) ERR_CANNOT_BID_OWN_AUCTION)
    (asserts! (>= bid-amount minimum-bid) ERR_BID_TOO_LOW)
    (asserts! (>= (stx-get-balance tx-sender) bid-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
    
    (map-set bidder-escrow
      { property-id: property-id, bidder: tx-sender }
      { amount: total-escrow }
    )
    
    (map-set auction-bids
      { property-id: property-id, bid-id: (get bid-count auction) }
      {
        bidder: tx-sender,
        amount: bid-amount,
        timestamp: current-block,
        refunded: false
      }
    )
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction {
        current-bid: bid-amount,
        highest-bidder: (some tx-sender),
        bid-count: (+ (get bid-count auction) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (finalize-auction (property-id uint))
  (let
    (
      (auction (unwrap! (map-get? property-auctions { property-id: property-id }) ERR_AUCTION_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (highest-bidder (get highest-bidder auction))
      (winning-bid (get current-bid auction))
      (reserve-price (get reserve-price auction))
      (seller (get seller auction))
    )
    (asserts! (get active auction) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (> current-block (get end-time auction)) ERR_AUCTION_NOT_ENDED)
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction { active: false })
    )
    
    (if (and (is-some highest-bidder) (>= winning-bid reserve-price))
      (begin
        (try! (as-contract (stx-transfer? winning-bid tx-sender seller)))
        
        (map-set properties
          { property-id: property-id }
          (merge property {
            owner: (unwrap-panic highest-bidder),
            last-transfer: current-block,
            value: winning-bid,
            for-auction: false
          })
        )
        
        (map-set property-history
          { property-id: property-id, transfer-id: (get count (default-to { count: u0 } (map-get? transfer-counter { property-id: property-id }))) }
          {
            from: seller,
            to: (unwrap-panic highest-bidder),
            price: winning-bid,
            timestamp: current-block,
            transfer-type: "auction"
          }
        )
        
        (map-set transfer-counter
          { property-id: property-id }
          { count: (+ (get count (default-to { count: u0 } (map-get? transfer-counter { property-id: property-id }))) u1) }
        )
        
        (remove-property-from-owner seller property-id)
        (update-owner-properties (unwrap-panic highest-bidder) property-id)
        
        (ok true)
      )
      (begin
        (map-set properties
          { property-id: property-id }
          (merge property { for-auction: false })
        )
        (ok false)
      )
    )
  )
)

(define-public (cancel-auction (property-id uint))
  (let
    (
      (auction (unwrap! (map-get? property-auctions { property-id: property-id }) ERR_AUCTION_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq (get seller auction) tx-sender) ERR_NOT_OWNER)
    (asserts! (get active auction) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (is-eq (get bid-count auction) u0) ERR_NO_BIDS_PLACED)
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction { active: false })
    )
    
    (map-set properties
      { property-id: property-id }
      (merge property { for-auction: false })
    )
    
    (ok true)
  )
)

(define-public (refund-losing-bid (property-id uint) (bidder principal))
  (let
    (
      (auction (unwrap! (map-get? property-auctions { property-id: property-id }) ERR_AUCTION_NOT_FOUND))
      (escrow (unwrap! (map-get? bidder-escrow { property-id: property-id, bidder: bidder }) ERR_INSUFFICIENT_FUNDS))
      (highest-bidder (get highest-bidder auction))
      (escrow-amount (get amount escrow))
    )
    (asserts! (not (get active auction)) ERR_AUCTION_NOT_ENDED)
    (asserts! (> escrow-amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (or (is-none highest-bidder) (not (is-eq (unwrap-panic highest-bidder) bidder))) ERR_RENT_ALREADY_PAID)
    
    (try! (as-contract (stx-transfer? escrow-amount tx-sender bidder)))
    
    (map-delete bidder-escrow { property-id: property-id, bidder: bidder })
    
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

(define-read-only (get-rental-agreement (property-id uint))
  (map-get? rental-agreements { property-id: property-id })
)

(define-read-only (get-rental-payment (property-id uint) (payment-id uint))
  (map-get? rental-payments { property-id: property-id, payment-id: payment-id })
)

(define-read-only (get-rental-payment-count (property-id uint))
  (match (map-get? rental-payment-counter { property-id: property-id })
    counter (get count counter)
    u0
  )
)

(define-read-only (is-rent-overdue (property-id uint))
  (match (map-get? rental-agreements { property-id: property-id })
    rental (and (get active rental) (> stacks-block-height (get next-payment-due rental)))
    false
  )
)

(define-read-only (get-rent-due-date (property-id uint))
  (match (map-get? rental-agreements { property-id: property-id })
    rental (some (get next-payment-due rental))
    none
  )
)

(define-read-only (is-property-for-rent (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (get for-rent property)
    false
  )
)

(define-read-only (get-rental-price (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (get rental-price property)
    u0
  )
)

(define-read-only (get-auction (property-id uint))
  (map-get? property-auctions { property-id: property-id })
)

(define-read-only (get-auction-bid (property-id uint) (bid-id uint))
  (map-get? auction-bids { property-id: property-id, bid-id: bid-id })
)

(define-read-only (get-bidder-escrow (property-id uint) (bidder principal))
  (map-get? bidder-escrow { property-id: property-id, bidder: bidder })
)

(define-read-only (is-auction-active (property-id uint))
  (match (map-get? property-auctions { property-id: property-id })
    auction (and (get active auction) (<= stacks-block-height (get end-time auction)))
    false
  )
)

(define-read-only (is-auction-ended (property-id uint))
  (match (map-get? property-auctions { property-id: property-id })
    auction (> stacks-block-height (get end-time auction))
    false
  )
)

(define-read-only (get-current-bid (property-id uint))
  (match (map-get? property-auctions { property-id: property-id })
    auction (get current-bid auction)
    u0
  )
)

(define-read-only (get-highest-bidder (property-id uint))
  (match (map-get? property-auctions { property-id: property-id })
    auction (get highest-bidder auction)
    none
  )
)

(define-read-only (is-property-for-auction (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property (get for-auction property)
    false
  )
)

(define-read-only (get-auction-time-remaining (property-id uint))
  (match (map-get? property-auctions { property-id: property-id })
    auction (if (> (get end-time auction) stacks-block-height) 
              (some (- (get end-time auction) stacks-block-height)) 
              (some u0))
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
    )
    (var-set target-property-id property-id)
    (let
      (
        (filtered-properties (filter is-not-target-property current-properties))
      )
      (map-set owner-properties
        { owner: owner }
        { property-ids: filtered-properties }
      )
    )
  )
)

(define-data-var target-property-id uint u0)

(define-private (is-not-target-property (id uint))
  (not (is-eq id (var-get target-property-id)))
)



