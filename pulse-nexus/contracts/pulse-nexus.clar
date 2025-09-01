;; PulseNexus - Revolutionary DAO Governance Platform with Dynamic Committee Formation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-PROPOSAL-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-CONSENSUS-NOT-READY (err u105))
(define-constant ERR-ALREADY-VALIDATED (err u106))
(define-constant ERR-PROPOSAL-COMPLETED (err u107))
(define-constant ERR-INVALID-STATUS (err u108))

;; Data Variables
(define-data-var next-proposal-id uint u1)
(define-data-var governance-fee uint u250) ;; 2.5% in basis points (250/10000)
(define-data-var total-platform-revenue uint u0)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-utf8 200),
    description: (string-utf8 500),
    stake-requirement: uint,
    current-stake: uint,
    domain: (string-ascii 50),
    status: (string-ascii 20), ;; "evaluation", "active", "completed", "failed"
    created-at: uint,
    consensus-phases: uint,
    total-phases: uint
  }
)

(define-map member-stakes
  { member: principal, proposal-id: uint }
  {
    amount: uint,
    staked-at: uint
  }
)

(define-map member-reputation
  { member: principal }
  {
    score: uint,
    successful-proposals: uint,
    total-staked: uint
  }
)

(define-map consensus-releases
  { proposal-id: uint, phase: uint }
  {
    amount: uint,
    released-at: uint,
    is-released: bool
  }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-proposal-creator (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (is-eq tx-sender (get creator proposal))
    false
  )
)

;; Helper Functions
(define-private (calculate-governance-fee (amount uint))
  (/ (* amount (var-get governance-fee)) u10000)
)

(define-private (update-member-reputation (member principal) (amount uint) (success bool))
  (let (
    (current-rep (default-to 
      { score: u100, successful-proposals: u0, total-staked: u0 }
      (map-get? member-reputation { member: member })
    ))
  )
    (map-set member-reputation
      { member: member }
      {
        score: (if success 
          (+ (get score current-rep) u10) 
          (if (> (get score current-rep) u10) (- (get score current-rep) u10) u0)
        ),
        successful-proposals: (if success (+ (get successful-proposals current-rep) u1) (get successful-proposals current-rep)),
        total-staked: (+ (get total-staked current-rep) amount)
      }
    )
  )
)

;; Core Functions
(define-public (create-proposal 
  (title (string-utf8 200))
  (description (string-utf8 500))
  (stake-requirement uint)
  (domain (string-ascii 50))
  (total-phases uint))
  (let ((proposal-id (var-get next-proposal-id)))
    (asserts! (> stake-requirement u0) ERR-INVALID-AMOUNT)
    (asserts! (> total-phases u0) ERR-INVALID-AMOUNT)
    (asserts! (< (len title) u201) ERR-INVALID-AMOUNT)
    (asserts! (< (len description) u501) ERR-INVALID-AMOUNT)
    (asserts! (< (len domain) u51) ERR-INVALID-AMOUNT)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        stake-requirement: stake-requirement,
        current-stake: u0,
        domain: domain,
        status: "evaluation",
        created-at: block-height,
        consensus-phases: u0,
        total-phases: total-phases
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (stake-on-proposal (proposal-id uint) (amount uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (fee (calculate-governance-fee amount))
    (net-amount (- amount fee))
    (current-stake (default-to 
      { amount: u0, staked-at: u0 }
      (map-get? member-stakes { member: tx-sender, proposal-id: proposal-id })
    ))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status proposal) "evaluation") ERR-PROPOSAL-COMPLETED)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update platform revenue
    (var-set total-platform-revenue (+ (var-get total-platform-revenue) fee))
    
    ;; Update proposal stake
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { current-stake: (+ (get current-stake proposal) net-amount) })
    )
    
    ;; Update member stake
    (map-set member-stakes
      { member: tx-sender, proposal-id: proposal-id }
      {
        amount: (+ (get amount current-stake) net-amount),
        staked-at: block-height
      }
    )
    
    ;; Update member reputation
    (update-member-reputation tx-sender net-amount true)
    
    ;; Check if stake requirement is reached
    (if (>= (+ (get current-stake proposal) net-amount) (get stake-requirement proposal))
      (begin
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { 
            current-stake: (+ (get current-stake proposal) net-amount),
            status: "active" 
          })
        )
        (ok { proposal-activated: true, stake-amount: net-amount })
      )
      (ok { proposal-activated: false, stake-amount: net-amount })
    )
  )
)

(define-public (release-consensus-funding (proposal-id uint) (phase uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (phase-amount (/ (get current-stake proposal) (get total-phases proposal)))
  )
    (asserts! (is-proposal-creator proposal-id) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status proposal) "active") ERR-INVALID-STATUS)
    (asserts! (< phase (get total-phases proposal)) ERR-CONSENSUS-NOT-READY)
    (asserts! (is-eq phase (get consensus-phases proposal)) ERR-CONSENSUS-NOT-READY)
    (asserts! 
      (is-none (map-get? consensus-releases { proposal-id: proposal-id, phase: phase }))
      ERR-ALREADY-VALIDATED
    )
    
    ;; Release phase funding
    (try! (as-contract (stx-transfer? phase-amount tx-sender (get creator proposal))))
    
    ;; Record consensus release
    (map-set consensus-releases
      { proposal-id: proposal-id, phase: phase }
      {
        amount: phase-amount,
        released-at: block-height,
        is-released: true
      }
    )
    
    ;; Update proposal phases
    (let ((new-phases-completed (+ (get consensus-phases proposal) u1)))
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { consensus-phases: new-phases-completed })
      )
      
      ;; Check if proposal is completed
      (if (is-eq new-phases-completed (get total-phases proposal))
        (begin
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { 
              consensus-phases: new-phases-completed,
              status: "completed" 
            })
          )
          (reward-committee-members proposal-id)
          (ok { phase-released: phase-amount, proposal-completed: true })
        )
        (ok { phase-released: phase-amount, proposal-completed: false })
      )
    )
  )
)

(define-private (reward-committee-members (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) false)))
    ;; Simple reward mechanism - in a real implementation, you'd iterate through committee members
    ;; For now, just update the creator's reputation
    (update-member-reputation (get creator proposal) (get current-stake proposal) true)
    true
  )
)

;; Read-only Functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-stake (member principal) (proposal-id uint))
  (map-get? member-stakes { member: member, proposal-id: proposal-id })
)

(define-read-only (get-member-reputation (member principal))
  (map-get? member-reputation { member: member })
)

(define-read-only (get-platform-stats)
  {
    total-proposals: (- (var-get next-proposal-id) u1),
    governance-fee: (var-get governance-fee),
    total-revenue: (var-get total-platform-revenue)
  }
)

(define-read-only (get-consensus-info (proposal-id uint) (phase uint))
  (map-get? consensus-releases { proposal-id: proposal-id, phase: phase })
)

(define-read-only (calculate-proposal-progress (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (some {
      stake-progress: (if (> (get stake-requirement proposal) u0)
        (/ (* (get current-stake proposal) u100) (get stake-requirement proposal))
        u0
      ),
      consensus-progress: (if (> (get total-phases proposal) u0)
        (/ (* (get consensus-phases proposal) u100) (get total-phases proposal))
        u0
      )
    })
    none
  )
)

;; Admin Functions
(define-public (update-governance-fee (new-fee uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT) ;; Max 10% fee
    (var-set governance-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-platform-revenue (amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= amount (var-get total-platform-revenue)) ERR-INSUFFICIENT-FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (var-set total-platform-revenue (- (var-get total-platform-revenue) amount))
    (ok amount)
  )
)

;; Emergency Functions
(define-public (emergency-pause-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: "failed" })
    )
    (ok true)
  )
)

;; Utility Functions
(define-private (verify-triple-validation (proposal-id uint))
  ;; Simplified validation - in production, this would check multiple committee validations
  true
)

(define-private (update-domain-pool (domain (string-ascii 50)) (amount uint))
  ;; Simplified domain pool update
  true
)