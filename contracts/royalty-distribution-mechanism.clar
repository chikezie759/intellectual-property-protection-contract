;; Royalty Distribution Mechanism Smart Contract
;; Automated royalty distribution and licensing platform

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_LICENSE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_PERCENTAGE (err u202))
(define-constant ERR_INSUFFICIENT_BALANCE (err u203))
(define-constant ERR_PAYMENT_FAILED (err u204))
(define-constant ERR_LICENSE_EXPIRED (err u205))
(define-constant ERR_ALREADY_LICENSED (err u206))
(define-constant ERR_INVALID_DURATION (err u207))
(define-constant ERR_DISTRIBUTION_FAILED (err u208))
(define-constant ERR_NO_ROYALTIES_DUE (err u209))

;; Data Variables
(define-data-var license-id-nonce uint u0)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee
(define-data-var minimum-license-fee uint u100000) ;; 0.1 STX
(define-data-var distribution-threshold uint u1000000) ;; 1 STX minimum for distribution
(define-data-var contract-paused bool false)

;; Data Structures
(define-map licenses
  { license-id: uint }
  {
    licensor: principal,
    licensee: principal,
    ip-asset-id: uint,
    license-type: (string-ascii 50),
    fee-amount: uint,
    royalty-percentage: uint,
    start-date: uint,
    end-date: uint,
    usage-limit: (optional uint),
    current-usage: uint,
    status: (string-ascii 20),
    territory: (string-utf8 100),
    exclusive: bool
  }
)

(define-map royalty-beneficiaries
  { ip-asset-id: uint }
  {
    primary-owner: principal,
    secondary-beneficiaries: (list 10 { beneficiary: principal, percentage: uint }),
    total-allocated: uint
  }
)

(define-map usage-reports
  { license-id: uint, report-id: uint }
  {
    reporter: principal,
    usage-amount: uint,
    revenue-generated: uint,
    report-date: uint,
    verified: bool,
    verification-date: (optional uint)
  }
)

(define-map accumulated-royalties
  { beneficiary: principal }
  {
    total-earned: uint,
    total-withdrawn: uint,
    pending-amount: uint,
    last-distribution: uint
  }
)

(define-map license-agreements
  { license-id: uint }
  {
    terms: (string-utf8 2048),
    payment-schedule: (string-ascii 50),
    renewal-terms: (optional (string-utf8 512)),
    penalty-clauses: (optional (string-utf8 512))
  }
)

(define-map ip-asset-metadata
  { ip-asset-id: uint }
  {
    asset-type: (string-ascii 50),
    title: (string-utf8 256),
    description: (string-utf8 1024),
    creation-date: uint,
    owner: principal,
    total-licenses: uint,
    total-revenue: uint
  }
)

(define-map payment-schedules
  { license-id: uint, payment-id: uint }
  {
    due-date: uint,
    amount: uint,
    paid: bool,
    payment-date: (optional uint),
    late-fee: uint
  }
)

;; Read-only functions
(define-read-only (get-license (license-id uint))
  (map-get? licenses { license-id: license-id })
)

(define-read-only (get-royalty-beneficiaries (ip-asset-id uint))
  (map-get? royalty-beneficiaries { ip-asset-id: ip-asset-id })
)

(define-read-only (get-usage-report (license-id uint) (report-id uint))
  (map-get? usage-reports { license-id: license-id, report-id: report-id })
)

(define-read-only (get-accumulated-royalties (beneficiary principal))
  (default-to
    {
      total-earned: u0,
      total-withdrawn: u0,
      pending-amount: u0,
      last-distribution: u0
    }
    (map-get? accumulated-royalties { beneficiary: beneficiary })
  )
)

(define-read-only (get-license-agreement (license-id uint))
  (map-get? license-agreements { license-id: license-id })
)

(define-read-only (get-ip-asset-metadata (ip-asset-id uint))
  (map-get? ip-asset-metadata { ip-asset-id: ip-asset-id })
)

(define-read-only (get-current-license-id)
  (var-get license-id-nonce)
)

(define-read-only (calculate-royalty-amount (revenue uint) (percentage uint))
  (/ (* revenue percentage) u10000) ;; percentage in basis points
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

(define-read-only (is-license-active (license-id uint))
  (match (get-license license-id)
    license-info
      (and 
        (> (get end-date license-info) block-height)
        (is-eq (get status license-info) "active")
      )
    false
  )
)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (increment-license-id)
  (let (
    (current-id (var-get license-id-nonce))
    (new-id (+ current-id u1))
  )
  (var-set license-id-nonce new-id)
  new-id
  )
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u100)
)

(define-private (validate-percentage-allocation (beneficiaries (list 10 { beneficiary: principal, percentage: uint })))
  (let (
    (total-percentage (fold + (map get-percentage beneficiaries) u0))
  )
  (<= total-percentage u10000) ;; Max 100% in basis points
  )
)

(define-private (get-percentage (beneficiary-info { beneficiary: principal, percentage: uint }))
  (get percentage beneficiary-info)
)

(define-private (distribute-to-beneficiary (beneficiary-info { beneficiary: principal, percentage: uint }) (total-amount uint))
  (let (
    (beneficiary (get beneficiary beneficiary-info))
    (percentage (get percentage beneficiary-info))
    (amount (/ (* total-amount percentage) u10000))
    (current-royalties (get-accumulated-royalties beneficiary))
  )
  (if (> amount u0)
    (map-set accumulated-royalties
      { beneficiary: beneficiary }
      {
        total-earned: (+ (get total-earned current-royalties) amount),
        total-withdrawn: (get total-withdrawn current-royalties),
        pending-amount: (+ (get pending-amount current-royalties) amount),
        last-distribution: block-height
      }
    )
    false
  )
  true
  )
)

(define-private (distribute-to-beneficiary-fold (beneficiary-info { beneficiary: principal, percentage: uint }) (total-amount uint))
  (begin
    (distribute-to-beneficiary beneficiary-info total-amount)
    total-amount
  )
)

;; Public functions
(define-public (create-license
  (licensee principal)
  (ip-asset-id uint)
  (license-type (string-ascii 50))
  (fee-amount uint)
  (royalty-percentage uint)
  (duration-blocks uint)
  (usage-limit (optional uint))
  (territory (string-utf8 100))
  (exclusive bool)
  (terms (string-utf8 2048))
  )
  (let (
    (license-id (increment-license-id))
    (start-date block-height)
    (end-date (+ block-height duration-blocks))
  )
  (asserts! (not (var-get contract-paused)) (err u999))
  (asserts! (>= fee-amount (var-get minimum-license-fee)) (err u210))
  (asserts! (<= royalty-percentage u10000) ERR_INVALID_PERCENTAGE) ;; Max 100%
  (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
  
  ;; Create the license
  (map-set licenses
    { license-id: license-id }
    {
      licensor: tx-sender,
      licensee: licensee,
      ip-asset-id: ip-asset-id,
      license-type: license-type,
      fee-amount: fee-amount,
      royalty-percentage: royalty-percentage,
      start-date: start-date,
      end-date: end-date,
      usage-limit: usage-limit,
      current-usage: u0,
      status: "pending",
      territory: territory,
      exclusive: exclusive
    }
  )
  
  ;; Store license agreement terms
  (map-set license-agreements
    { license-id: license-id }
    {
      terms: terms,
      payment-schedule: "upfront",
      renewal-terms: none,
      penalty-clauses: none
    }
  )
  
  (ok license-id)
  )
)

(define-public (accept-license (license-id uint))
  (let (
    (license-info (unwrap! (get-license license-id) ERR_LICENSE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get licensee license-info)) ERR_NOT_AUTHORIZED)
  (asserts! (is-eq (get status license-info) "pending") (err u211))
  (asserts! (>= (stx-get-balance tx-sender) (get fee-amount license-info)) ERR_INSUFFICIENT_BALANCE)
  
  ;; Calculate platform fee
  (let (
    (fee-amount (get fee-amount license-info))
    (platform-fee (calculate-platform-fee fee-amount))
    (licensor-amount (- fee-amount platform-fee))
  )
  
  ;; Transfer license fee
  (try! (stx-transfer? licensor-amount tx-sender (get licensor license-info)))
  (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
  
  ;; Update license status
  (map-set licenses
    { license-id: license-id }
    (merge license-info { status: "active" })
  )
  
  (ok true)
  )
  )
)

(define-public (setup-royalty-beneficiaries
  (ip-asset-id uint)
  (secondary-beneficiaries (list 10 { beneficiary: principal, percentage: uint }))
  )
  (begin
  (asserts! (validate-percentage-allocation secondary-beneficiaries) ERR_INVALID_PERCENTAGE)
  
  ;; Calculate total allocation
  (let (
    (total-allocated (fold + (map get-percentage secondary-beneficiaries) u0))
  )
  
  (map-set royalty-beneficiaries
    { ip-asset-id: ip-asset-id }
    {
      primary-owner: tx-sender,
      secondary-beneficiaries: secondary-beneficiaries,
      total-allocated: total-allocated
    }
  )
  
  (ok true)
  )
  )
)

(define-public (submit-usage-report
  (license-id uint)
  (report-id uint)
  (usage-amount uint)
  (revenue-generated uint)
  )
  (let (
    (license-info (unwrap! (get-license license-id) ERR_LICENSE_NOT_FOUND))
  )
  (asserts! (is-eq tx-sender (get licensee license-info)) ERR_NOT_AUTHORIZED)
  (asserts! (is-license-active license-id) ERR_LICENSE_EXPIRED)
  
  ;; Record usage report
  (map-set usage-reports
    { license-id: license-id, report-id: report-id }
    {
      reporter: tx-sender,
      usage-amount: usage-amount,
      revenue-generated: revenue-generated,
      report-date: block-height,
      verified: false,
      verification-date: none
    }
  )
  
  ;; Update license usage
  (let (
    (new-usage (+ (get current-usage license-info) usage-amount))
  )
  (map-set licenses
    { license-id: license-id }
    (merge license-info { current-usage: new-usage })
  )
  )
  
  (ok true)
  )
)

(define-public (distribute-royalties
  (license-id uint)
  (report-id uint)
  )
  (let (
    (license-info (unwrap! (get-license license-id) ERR_LICENSE_NOT_FOUND))
    (usage-report (unwrap! (get-usage-report license-id report-id) (err u212)))
    (royalty-info (get-royalty-beneficiaries (get ip-asset-id license-info)))
  )
  (asserts! (get verified usage-report) (err u213))
  (asserts! (> (get revenue-generated usage-report) u0) ERR_NO_ROYALTIES_DUE)
  
  (let (
    (total-revenue (get revenue-generated usage-report))
    (royalty-amount (calculate-royalty-amount total-revenue (get royalty-percentage license-info)))
  )
  
  (asserts! (>= royalty-amount (var-get distribution-threshold)) (err u214))
  
  ;; Distribute to beneficiaries if configured
  (match royalty-info
    beneficiary-info
      (begin
        ;; Distribute to secondary beneficiaries
        (let (
          (beneficiaries-list (get secondary-beneficiaries beneficiary-info))
        )
        (fold distribute-to-beneficiary-fold beneficiaries-list royalty-amount)
        )
        
        ;; Remaining goes to primary owner
        (let (
          (remaining-percentage (- u10000 (get total-allocated beneficiary-info)))
          (primary-amount (/ (* royalty-amount remaining-percentage) u10000))
          (primary-owner (get primary-owner beneficiary-info))
          (current-royalties (get-accumulated-royalties primary-owner))
        )
        (map-set accumulated-royalties
          { beneficiary: primary-owner }
          {
            total-earned: (+ (get total-earned current-royalties) primary-amount),
            total-withdrawn: (get total-withdrawn current-royalties),
            pending-amount: (+ (get pending-amount current-royalties) primary-amount),
            last-distribution: block-height
          }
        )
        )
      )
    ;; No beneficiaries configured, all goes to licensor
    (let (
      (licensor (get licensor license-info))
      (current-royalties (get-accumulated-royalties licensor))
    )
    (map-set accumulated-royalties
      { beneficiary: licensor }
      {
        total-earned: (+ (get total-earned current-royalties) royalty-amount),
        total-withdrawn: (get total-withdrawn current-royalties),
        pending-amount: (+ (get pending-amount current-royalties) royalty-amount),
        last-distribution: block-height
      }
    )
    )
  )
  
  (ok royalty-amount)
  )
  )
)

(define-public (withdraw-royalties)
  (let (
    (beneficiary-royalties (get-accumulated-royalties tx-sender))
    (pending-amount (get pending-amount beneficiary-royalties))
  )
  (asserts! (> pending-amount u0) ERR_NO_ROYALTIES_DUE)
  (asserts! (>= (stx-get-balance (as-contract tx-sender)) pending-amount) ERR_INSUFFICIENT_BALANCE)
  
  ;; Transfer royalties
  (try! (as-contract (stx-transfer? pending-amount tx-sender tx-sender)))
  
  ;; Update royalty records
  (map-set accumulated-royalties
    { beneficiary: tx-sender }
    {
      total-earned: (get total-earned beneficiary-royalties),
      total-withdrawn: (+ (get total-withdrawn beneficiary-royalties) pending-amount),
      pending-amount: u0,
      last-distribution: (get last-distribution beneficiary-royalties)
    }
  )
  
  (ok pending-amount)
  )
)

(define-public (verify-usage-report (license-id uint) (report-id uint) (is-valid bool))
  (let (
    (license-info (unwrap! (get-license license-id) ERR_LICENSE_NOT_FOUND))
    (usage-report (unwrap! (get-usage-report license-id report-id) (err u212)))
  )
  (asserts! (is-eq tx-sender (get licensor license-info)) ERR_NOT_AUTHORIZED)
  (asserts! (not (get verified usage-report)) (err u215))
  
  ;; Update verification status
  (map-set usage-reports
    { license-id: license-id, report-id: report-id }
    (merge usage-report {
      verified: is-valid,
      verification-date: (some block-height)
    })
  )
  
  (ok true)
  )
)

;; Admin functions
(define-public (set-platform-fee-percentage (new-percentage uint))
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (asserts! (<= new-percentage u50) ERR_INVALID_PERCENTAGE) ;; Max 50%
  (var-set platform-fee-percentage new-percentage)
  (ok true)
  )
)

(define-public (set-minimum-license-fee (new-fee uint))
  (begin
  (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
  (var-set minimum-license-fee new-fee)
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


;; title: royalty-distribution-mechanism
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

