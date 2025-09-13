;; Patent Registration System Smart Contract
;; Decentralized patent registration and validation platform

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PATENT_NOT_FOUND (err u101))
(define-constant ERR_PATENT_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PATENT_ID (err u103))
(define-constant ERR_PATENT_EXPIRED (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_VALIDATION_FAILED (err u106))
(define-constant ERR_ALREADY_VALIDATED (err u107))
(define-constant ERR_INVALID_STATUS (err u108))

;; Data Variables
(define-data-var patent-id-nonce uint u0)
(define-data-var registration-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var validation-period uint u52560) ;; Approximately 1 year in blocks
(define-data-var contract-paused bool false)

;; Data Structures
(define-map patents
  { patent-id: uint }
  {
    inventor: principal,
    title: (string-utf8 256),
    description: (string-utf8 1024),
    patent-hash: (buff 32),
    registration-date: uint,
    expiration-date: uint,
    status: (string-ascii 20),
    validation-count: uint,
    license-fee: uint,
    is-renewable: bool
  }
)

(define-map patent-validations
  { patent-id: uint, validator: principal }
  {
    validation-date: uint,
    is-valid: bool,
    comments: (string-utf8 512)
  }
)

(define-map inventor-patents
  { inventor: principal }
  { patent-ids: (list 100 uint) }
)

(define-map prior-art
  { content-hash: (buff 32) }
  {
    patent-id: uint,
    submission-date: uint,
    submitter: principal
  }
)

(define-map authorized-validators
  { validator: principal }
  {
    authorized-date: uint,
    validation-count: uint,
    reputation-score: uint
  }
)

;; Patent classification system
(define-map patent-classifications
  { patent-id: uint }
  {
    primary-class: (string-ascii 10),
    secondary-classes: (list 5 (string-ascii 10)),
    technology-field: (string-utf8 128)
  }
)

;; Read-only functions
(define-read-only (get-patent (patent-id uint))
  (map-get? patents { patent-id: patent-id })
)

(define-read-only (get-patent-validation (patent-id uint) (validator principal))
  (map-get? patent-validations { patent-id: patent-id, validator: validator })
)

(define-read-only (get-inventor-patents (inventor principal))
  (default-to 
    { patent-ids: (list) }
    (map-get? inventor-patents { inventor: inventor })
  )
)

(define-read-only (get-prior-art (content-hash (buff 32)))
  (map-get? prior-art { content-hash: content-hash })
)

(define-read-only (is-authorized-validator (validator principal))
  (is-some (map-get? authorized-validators { validator: validator }))
)

(define-read-only (get-registration-fee)
  (var-get registration-fee)
)

(define-read-only (get-current-patent-id)
  (var-get patent-id-nonce)
)

(define-read-only (is-patent-expired (patent-id uint))
  (match (get-patent patent-id)
    patent-info 
      (> block-height (get expiration-date patent-info))
    false
  )
)

(define-read-only (calculate-patent-value (patent-id uint))
  (match (get-patent patent-id)
    patent-info
      (let (
        (base-value (get license-fee patent-info))
        (validation-bonus (* (get validation-count patent-info) u50000))
        (time-factor (if (> (get expiration-date patent-info) block-height) u100 u50))
      )
      (some (+ base-value validation-bonus (/ (* base-value time-factor) u100)))
    )
    none
  )
)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (increment-patent-id)
  (let (
    (current-id (var-get patent-id-nonce))
    (new-id (+ current-id u1))
  )
  (var-set patent-id-nonce new-id)
  new-id
  )
)

(define-private (add-to-inventor-patents (inventor principal) (patent-id uint))
  (let (
    (current-patents (get patent-ids (get-inventor-patents inventor)))
    (updated-patents (unwrap-panic (as-max-len? (append current-patents patent-id) u100)))
  )
  (map-set inventor-patents 
    { inventor: inventor }
    { patent-ids: updated-patents }
  )
  )
)

(define-private (check-prior-art (content-hash (buff 32)))
  (is-none (get-prior-art content-hash))
)

;; Public functions
(define-public (register-patent 
  (title (string-utf8 256))
  (description (string-utf8 1024))
  (patent-hash (buff 32))
  (license-fee uint)
  (primary-class (string-ascii 10))
  (secondary-classes (list 5 (string-ascii 10)))
  (technology-field (string-utf8 128))
  )
  (let (
    (patent-id (increment-patent-id))
    (registration-date block-height)
    (expiration-date (+ block-height (var-get validation-period)))
  )
  (asserts! (not (var-get contract-paused)) (err u999))
  (asserts! (check-prior-art patent-hash) ERR_PATENT_ALREADY_EXISTS)
  (asserts! (>= (stx-get-balance tx-sender) (var-get registration-fee)) ERR_INSUFFICIENT_BALANCE)
  
  ;; Transfer registration fee
  (try! (stx-transfer? (var-get registration-fee) tx-sender CONTRACT_OWNER))
  
  ;; Register the patent
  (map-set patents
    { patent-id: patent-id }
    {
      inventor: tx-sender,
      title: title,
      description: description,
      patent-hash: patent-hash,
      registration-date: registration-date,
      expiration-date: expiration-date,
      status: "pending",
      validation-count: u0,
      license-fee: license-fee,
      is-renewable: true
    }
  )
  
  ;; Record prior art
  (map-set prior-art
    { content-hash: patent-hash }
    {
      patent-id: patent-id,
      submission-date: registration-date,
      submitter: tx-sender
    }
  )
  
  ;; Add classification
  (map-set patent-classifications
    { patent-id: patent-id }
    {
      primary-class: primary-class,
      secondary-classes: secondary-classes,
      technology-field: technology-field
    }
  )
  
  ;; Add to inventor's patent list
  (add-to-inventor-patents tx-sender patent-id)
  
  (ok patent-id)
  )
)

(define-public (validate-patent (patent-id uint) (is-valid bool) (comments (string-utf8 512)))
  (let (
    (patent-info (unwrap! (get-patent patent-id) ERR_PATENT_NOT_FOUND))
    (existing-validation (get-patent-validation patent-id tx-sender))
  )
  (asserts! (is-authorized-validator tx-sender) ERR_NOT_AUTHORIZED)
  (asserts! (is-none existing-validation) ERR_ALREADY_VALIDATED)
  (asserts! (not (is-patent-expired patent-id)) ERR_PATENT_EXPIRED)
  
  ;; Record validation
  (map-set patent-validations
    { patent-id: patent-id, validator: tx-sender }
    {
      validation-date: block-height,
      is-valid: is-valid,
      comments: comments
    }
  )
  
  ;; Update patent validation count
  (let (
    (new-validation-count (+ (get validation-count patent-info) u1))
    (new-status (if (and is-valid (>= new-validation-count u3)) "validated" "pending"))
  )
  (map-set patents
    { patent-id: patent-id }
    (merge patent-info { 
      validation-count: new-validation-count,
      status: new-status
    })
  )
  )
  
  ;; Update validator stats
  (match (map-get? authorized-validators { validator: tx-sender })
    validator-info
      (map-set authorized-validators
        { validator: tx-sender }
        (merge validator-info {
          validation-count: (+ (get validation-count validator-info) u1),
          reputation-score: (+ (get reputation-score validator-info) (if is-valid u10 u5))
        })
      )
    false
  )
  
  (ok true)
  )
)

(define-public (renew-patent (patent-id uint))
  (let (
    (patent-info (unwrap! (get-patent patent-id) ERR_PATENT_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get inventor patent-info)) ERR_NOT_AUTHORIZED)
  (asserts! (get is-renewable patent-info) (err u110))
  (asserts! (>= (stx-get-balance tx-sender) (var-get registration-fee)) ERR_INSUFFICIENT_BALANCE)
  
  ;; Transfer renewal fee
  (try! (stx-transfer? (var-get registration-fee) tx-sender CONTRACT_OWNER))
  
  ;; Extend expiration date
  (let (
    (new-expiration (+ (get expiration-date patent-info) (var-get validation-period)))
  )
  (map-set patents
    { patent-id: patent-id }
    (merge patent-info {
      expiration-date: new-expiration,
      status: "renewed"
    })
  )
  )
  
  (ok true)
  )
)

(define-public (transfer-patent (patent-id uint) (new-owner principal))
  (let (
    (patent-info (unwrap! (get-patent patent-id) ERR_PATENT_NOT_FOUND))
    (current-owner (get inventor patent-info))
  )
  (asserts! (is-eq tx-sender current-owner) ERR_NOT_AUTHORIZED)
  (asserts! (not (is-patent-expired patent-id)) ERR_PATENT_EXPIRED)
  
  ;; Update patent ownership
  (map-set patents
    { patent-id: patent-id }
    (merge patent-info { inventor: new-owner })
  )
  
  ;; Update inventor patent lists
  (add-to-inventor-patents new-owner patent-id)
  
  (ok true)
  )
)

;; Admin functions
(define-public (authorize-validator (validator principal))
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (map-set authorized-validators
    { validator: validator }
    {
      authorized-date: block-height,
      validation-count: u0,
      reputation-score: u100
    }
  )
  (ok true)
  )
)

(define-public (set-registration-fee (new-fee uint))
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (var-set registration-fee new-fee)
  (ok true)
  )
)

(define-public (pause-contract)
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (var-set contract-paused true)
  (ok true)
  )
)

(define-public (resume-contract)
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (var-set contract-paused false)
  (ok true)
  )
)


;; title: patent-registration-system
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

