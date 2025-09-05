;; Property Valuation & Assessment System for Landmark
;; Enables professional property assessments and valuation tracking

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_ASSESSOR_NOT_FOUND (err u201))
(define-constant ERR_ASSESSOR_ALREADY_EXISTS (err u202))
(define-constant ERR_ASSESSOR_NOT_VERIFIED (err u203))
(define-constant ERR_REQUEST_NOT_FOUND (err u204))
(define-constant ERR_REQUEST_ALREADY_EXISTS (err u205))
(define-constant ERR_INVALID_PROPERTY (err u206))
(define-constant ERR_INVALID_ASSESSMENT (err u207))
(define-constant ERR_ASSESSMENT_ALREADY_EXISTS (err u208))
(define-constant ERR_NOT_PROPERTY_OWNER (err u209))
(define-constant ERR_INVALID_FEE (err u210))
(define-constant ERR_INSUFFICIENT_FUNDS (err u211))

;; Contract owner (same as main Landmark contract)
(define-constant CONTRACT_OWNER tx-sender)

;; Data variables
(define-data-var next-request-id uint u1)
(define-data-var next-assessment-id uint u1)

;; Reference to main Landmark contract
(define-constant LANDMARK_CONTRACT .Landmark)

;; Registered assessors map
(define-map registered-assessors principal
  {
    name: (string-ascii 100),
    credentials: (string-ascii 200),
    license-number: (string-ascii 50),
    specialization: (string-ascii 100),
    fee-per-assessment: uint,
    registered-at: uint,
    verified: bool,
    total-assessments: uint,
    average-rating: uint
  }
)

;; Valuation requests map
(define-map valuation-requests uint
  {
    property-id: uint,
    requester: principal,
    requested-assessor: (optional principal),
    assessment-fee: uint,
    requested-at: uint,
    deadline: uint,
    purpose: (string-ascii 100),
    status: (string-ascii 20), ;; "open", "assigned", "completed", "cancelled"
    assigned-assessor: (optional principal)
  }
)

;; Property assessments map
(define-map property-assessments uint
  {
    request-id: uint,
    property-id: uint,
    assessor: principal,
    assessed-value: uint,
    assessment-date: uint,
    assessment-type: (string-ascii 50),
    methodology: (string-ascii 100),
    market-conditions: (string-ascii 100),
    comparable-properties: (string-ascii 200),
    notes: (string-ascii 300),
    confidence-rating: uint, ;; 1-10 scale
    valid-until: uint
  }
)

;; Assessment history tracking
(define-map property-assessment-history {property-id: uint, assessment-id: uint} bool)

;; Public functions

;; Register as a property assessor
(define-public (register-assessor 
    (name (string-ascii 100))
    (credentials (string-ascii 200))
    (license-number (string-ascii 50))
    (specialization (string-ascii 100))
    (fee-per-assessment uint))
  (let (
    (current-block stacks-block-height)
  )
    ;; Check assessor doesn't already exist
    (asserts! (is-none (map-get? registered-assessors tx-sender)) ERR_ASSESSOR_ALREADY_EXISTS)
    ;; Validate fee
    (asserts! (> fee-per-assessment u0) ERR_INVALID_FEE)
    
    ;; Register assessor
    (map-set registered-assessors tx-sender {
      name: name,
      credentials: credentials,
      license-number: license-number,
      specialization: specialization,
      fee-per-assessment: fee-per-assessment,
      registered-at: current-block,
      verified: false,
      total-assessments: u0,
      average-rating: u0
    })
    (ok true)
  )
)

;; Contract owner verifies assessor credentials
(define-public (verify-assessor (assessor principal))
  (let (
    (assessor-data (unwrap! (map-get? registered-assessors assessor) ERR_ASSESSOR_NOT_FOUND))
  )
    ;; Only contract owner can verify
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Update verification status
    (map-set registered-assessors assessor 
      (merge assessor-data {verified: true}))
    (ok true)
  )
)

;; Property owner requests valuation
(define-public (request-property-valuation
    (property-id uint)
    (requested-assessor (optional principal))
    (assessment-fee uint)
    (deadline uint)
    (purpose (string-ascii 100)))
  (let (
    (request-id (var-get next-request-id))
    (current-block stacks-block-height)
    (property-owner (unwrap! (contract-call? LANDMARK_CONTRACT get-property-owner property-id) ERR_INVALID_PROPERTY))
  )
    ;; Verify property exists and requester is owner
    (asserts! (is-eq tx-sender property-owner) ERR_NOT_PROPERTY_OWNER)
    ;; Check no existing open request for this property
    (asserts! (is-none (get-open-request-for-property property-id)) ERR_REQUEST_ALREADY_EXISTS)
    ;; Validate deadline is in future
    (asserts! (> deadline current-block) ERR_INVALID_ASSESSMENT)
    ;; Validate fee
    (asserts! (> assessment-fee u0) ERR_INVALID_FEE)
    
    ;; If specific assessor requested, verify they exist and are verified
    (if (is-some requested-assessor)
      (let (
        (assessor (unwrap-panic requested-assessor))
        (assessor-data (unwrap! (map-get? registered-assessors assessor) ERR_ASSESSOR_NOT_FOUND))
      )
        (asserts! (get verified assessor-data) ERR_ASSESSOR_NOT_VERIFIED)
        true
      )
      true
    )
    
    ;; Create valuation request
    (map-set valuation-requests request-id {
      property-id: property-id,
      requester: tx-sender,
      requested-assessor: requested-assessor,
      assessment-fee: assessment-fee,
      requested-at: current-block,
      deadline: deadline,
      purpose: purpose,
      status: "open",
      assigned-assessor: none
    })
    
    ;; Increment request ID
    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

;; Assessor accepts valuation request
(define-public (accept-valuation-request (request-id uint))
  (let (
    (request (unwrap! (map-get? valuation-requests request-id) ERR_REQUEST_NOT_FOUND))
    (assessor-data (unwrap! (map-get? registered-assessors tx-sender) ERR_ASSESSOR_NOT_FOUND))
    (current-block stacks-block-height)
  )
    ;; Verify assessor is verified
    (asserts! (get verified assessor-data) ERR_ASSESSOR_NOT_VERIFIED)
    ;; Verify request is still open
    (asserts! (is-eq (get status request) "open") ERR_INVALID_ASSESSMENT)
    ;; Check deadline hasn't passed
    (asserts! (< current-block (get deadline request)) ERR_INVALID_ASSESSMENT)
    
    ;; If specific assessor was requested, verify it's this assessor
    (if (is-some (get requested-assessor request))
      (asserts! (is-eq (unwrap-panic (get requested-assessor request)) tx-sender) ERR_UNAUTHORIZED)
      true
    )
    
    ;; Update request status
    (map-set valuation-requests request-id 
      (merge request {
        status: "assigned",
        assigned-assessor: (some tx-sender)
      }))
    (ok true)
  )
)

;; Assessor submits property assessment
(define-public (submit-assessment
    (request-id uint)
    (assessed-value uint)
    (assessment-type (string-ascii 50))
    (methodology (string-ascii 100))
    (market-conditions (string-ascii 100))
    (comparable-properties (string-ascii 200))
    (notes (string-ascii 300))
    (confidence-rating uint)
    (validity-months uint))
  (let (
    (assessment-id (var-get next-assessment-id))
    (request (unwrap! (map-get? valuation-requests request-id) ERR_REQUEST_NOT_FOUND))
    (assessor-data (unwrap! (map-get? registered-assessors tx-sender) ERR_ASSESSOR_NOT_FOUND))
    (current-block stacks-block-height)
    (valid-until (+ current-block (* validity-months u144 u30)))
    (property-id (get property-id request))
    (assessment-fee (get assessment-fee request))
    (requester (get requester request))
  )
    ;; Verify assessor is assigned to this request
    (asserts! (is-eq (some tx-sender) (get assigned-assessor request)) ERR_UNAUTHORIZED)
    ;; Verify request is assigned
    (asserts! (is-eq (get status request) "assigned") ERR_INVALID_ASSESSMENT)
    ;; Validate assessment inputs
    (asserts! (> assessed-value u0) ERR_INVALID_ASSESSMENT)
    (asserts! (and (>= confidence-rating u1) (<= confidence-rating u10)) ERR_INVALID_ASSESSMENT)
    (asserts! (> validity-months u0) ERR_INVALID_ASSESSMENT)
    ;; Check sufficient funds for fee payment
    (asserts! (>= (stx-get-balance requester) assessment-fee) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer assessment fee from requester to assessor
    (try! (stx-transfer? assessment-fee requester tx-sender))
    
    ;; Create assessment record
    (map-set property-assessments assessment-id {
      request-id: request-id,
      property-id: property-id,
      assessor: tx-sender,
      assessed-value: assessed-value,
      assessment-date: current-block,
      assessment-type: assessment-type,
      methodology: methodology,
      market-conditions: market-conditions,
      comparable-properties: comparable-properties,
      notes: notes,
      confidence-rating: confidence-rating,
      valid-until: valid-until
    })
    
    ;; Add to property assessment history
    (map-set property-assessment-history 
      {property-id: property-id, assessment-id: assessment-id} true)
    
    ;; Update request status
    (map-set valuation-requests request-id 
      (merge request {status: "completed"}))
    
    ;; Update assessor stats
    (map-set registered-assessors tx-sender
      (merge assessor-data {
        total-assessments: (+ (get total-assessments assessor-data) u1)
      }))
    
    ;; Increment assessment ID
    (var-set next-assessment-id (+ assessment-id u1))
    (ok assessment-id)
  )
)

;; Cancel valuation request
(define-public (cancel-valuation-request (request-id uint))
  (let (
    (request (unwrap! (map-get? valuation-requests request-id) ERR_REQUEST_NOT_FOUND))
  )
    ;; Only requester can cancel
    (asserts! (is-eq tx-sender (get requester request)) ERR_UNAUTHORIZED)
    ;; Can only cancel if open or assigned
    (asserts! (or (is-eq (get status request) "open") 
                  (is-eq (get status request) "assigned")) ERR_INVALID_ASSESSMENT)
    
    ;; Update status
    (map-set valuation-requests request-id 
      (merge request {status: "cancelled"}))
    (ok true)
  )
)

;; Read-only functions

;; Get assessor information
(define-read-only (get-assessor (assessor principal))
  (map-get? registered-assessors assessor)
)

;; Get valuation request
(define-read-only (get-valuation-request (request-id uint))
  (map-get? valuation-requests request-id)
)

;; Get property assessment
(define-read-only (get-property-assessment (assessment-id uint))
  (map-get? property-assessments assessment-id)
)

;; Check if property has open valuation request
(define-read-only (get-open-request-for-property (property-id uint))
  (let (
    (current-request-id (- (var-get next-request-id) u1))
  )
    (fold check-open-request 
      (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
      none)
  )
)

;; Get system info
(define-read-only (get-system-info)
  {
    next-request-id: (var-get next-request-id),
    next-assessment-id: (var-get next-assessment-id)
  }
)

;; Private helper function
(define-private (check-open-request (offset uint) (current-result (optional uint)))
  (if (is-some current-result)
    current-result
    (let (
      (request-id (- (var-get next-request-id) offset))
    )
      (if (> request-id u0)
        (match (map-get? valuation-requests request-id)
          request (if (is-eq (get status request) "open")
                    (some request-id)
                    none)
          none)
        none)
    )
  )
)
