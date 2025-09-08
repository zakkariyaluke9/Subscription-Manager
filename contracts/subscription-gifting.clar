(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u600))
(define-constant ERR_GIFT_NOT_FOUND (err u601))
(define-constant ERR_GIFT_EXPIRED (err u602))
(define-constant ERR_GIFT_ALREADY_CLAIMED (err u603))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u604))
(define-constant ERR_INVALID_RECIPIENT (err u605))
(define-constant ERR_GIFT_CODE_EXISTS (err u606))
(define-constant ERR_CANNOT_GIFT_SELF (err u607))

(define-data-var next-gift-id uint u1)
(define-data-var gift-expiration-blocks uint u26280)
(define-data-var platform-fee-percentage uint u3)
(define-data-var max-gift-value uint u10000000)
(define-data-var total-gifts-created uint u0)
(define-data-var total-gifts-claimed uint u0)

(define-map gift-cards
  { gift-id: uint }
  {
    gift-code: (string-ascii 16),
    gifter: principal,
    recipient: (optional principal),
    amount: uint,
    subscription-months: uint,
    is-claimed: bool,
    created-at: uint,
    expires-at: uint,
    claimed-at: uint,
    gift-message: (string-ascii 200),
    gift-type: (string-ascii 20)
  }
)

(define-map gift-code-lookup
  { gift-code: (string-ascii 16) }
  { gift-id: uint }
)

(define-map user-gift-stats
  { user: principal }
  {
    gifts-sent: uint,
    gifts-received: uint,
    total-gifted-amount: uint,
    total-received-amount: uint,
    last-gift-sent: uint,
    last-gift-received: uint
  }
)

(define-map gift-redemption-history
  { gift-id: uint }
  {
    redeemed-by: principal,
    redemption-block: uint,
    subscription-activated: bool,
    activation-details: (string-ascii 100)
  }
)

(define-map corporate-gift-batches
  { batch-id: uint }
  {
    company: principal,
    total-gifts: uint,
    amount-per-gift: uint,
    created-at: uint,
    batch-status: (string-ascii 20)
  }
)

(define-data-var next-batch-id uint u1)

(define-public (create-gift-card (gift-code (string-ascii 16)) (recipient (optional principal)) (subscription-months uint) (gift-message (string-ascii 200)))
  (let
    (
      (gift-id (var-get next-gift-id))
      (amount (* subscription-months u1000000))
      (platform-fee (/ (* amount (var-get platform-fee-percentage)) u100))
      (total-cost (+ amount platform-fee))
      (current-block stacks-block-height)
      (expiration-block (+ current-block (var-get gift-expiration-blocks)))
    )
    (asserts! (<= amount (var-get max-gift-value)) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (> subscription-months u0) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (is-none (map-get? gift-code-lookup { gift-code: gift-code })) ERR_GIFT_CODE_EXISTS)
    (match recipient
      some-recipient
        (asserts! (not (is-eq tx-sender some-recipient)) ERR_CANNOT_GIFT_SELF)
      true)
    
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (map-set gift-cards
      { gift-id: gift-id }
      {
        gift-code: gift-code,
        gifter: tx-sender,
        recipient: recipient,
        amount: amount,
        subscription-months: subscription-months,
        is-claimed: false,
        created-at: current-block,
        expires-at: expiration-block,
        claimed-at: u0,
        gift-message: gift-message,
        gift-type: "personal"
      }
    )
    
    (map-set gift-code-lookup { gift-code: gift-code } { gift-id: gift-id })
    
    (update-user-gift-stats tx-sender "sent" amount)
    (var-set next-gift-id (+ gift-id u1))
    (var-set total-gifts-created (+ (var-get total-gifts-created) u1))
    
    (ok {
      gift-id: gift-id,
      gift-code: gift-code,
      amount: amount,
      expires-at: expiration-block
    })
  )
)

(define-public (claim-gift (gift-code (string-ascii 16)))
  (let
    (
      (gift-lookup (unwrap! (map-get? gift-code-lookup { gift-code: gift-code }) ERR_GIFT_NOT_FOUND))
      (gift-id (get gift-id gift-lookup))
      (gift-card (unwrap! (map-get? gift-cards { gift-id: gift-id }) ERR_GIFT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (not (get is-claimed gift-card)) ERR_GIFT_ALREADY_CLAIMED)
    (asserts! (< current-block (get expires-at gift-card)) ERR_GIFT_EXPIRED)
    (match (get recipient gift-card)
      some-recipient
        (asserts! (is-eq tx-sender some-recipient) ERR_INVALID_RECIPIENT)
      true)
    
    (map-set gift-cards
      { gift-id: gift-id }
      (merge gift-card {
        is-claimed: true,
        claimed-at: current-block
      })
    )
    
    (map-set gift-redemption-history
      { gift-id: gift-id }
      {
        redeemed-by: tx-sender,
        redemption-block: current-block,
        subscription-activated: true,
        activation-details: "gift-subscription-activated"
      }
    )
    
    (update-user-gift-stats tx-sender "received" (get amount gift-card))
    (var-set total-gifts-claimed (+ (var-get total-gifts-claimed) u1))
    
    (ok {
      gift-id: gift-id,
      amount: (get amount gift-card),
      subscription-months: (get subscription-months gift-card),
      gifter: (get gifter gift-card)
    })
  )
)

(define-public (create-corporate-gift-batch (gifts-count uint) (subscription-months uint) (amount-per-gift uint))
  (let
    (
      (batch-id (var-get next-batch-id))
      (total-amount (* gifts-count amount-per-gift))
      (platform-fee (/ (* total-amount (var-get platform-fee-percentage)) u100))
      (total-cost (+ total-amount platform-fee))
    )
    (asserts! (> gifts-count u0) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (<= amount-per-gift (var-get max-gift-value)) ERR_INSUFFICIENT_PAYMENT)
    
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (map-set corporate-gift-batches
      { batch-id: batch-id }
      {
        company: tx-sender,
        total-gifts: gifts-count,
        amount-per-gift: amount-per-gift,
        created-at: stacks-block-height,
        batch-status: "active"
      }
    )
    
    (var-set next-batch-id (+ batch-id u1))
    
    (ok {
      batch-id: batch-id,
      total-gifts: gifts-count,
      total-cost: total-cost
    })
  )
)

(define-public (generate-batch-gift-code (batch-id uint) (gift-code (string-ascii 16)) (recipient (optional principal)) (gift-message (string-ascii 200)))
  (let
    (
      (batch (unwrap! (map-get? corporate-gift-batches { batch-id: batch-id }) ERR_GIFT_NOT_FOUND))
      (gift-id (var-get next-gift-id))
      (current-block stacks-block-height)
      (expiration-block (+ current-block (var-get gift-expiration-blocks)))
    )
    (asserts! (is-eq tx-sender (get company batch)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get batch-status batch) "active") ERR_GIFT_NOT_FOUND)
    (asserts! (is-none (map-get? gift-code-lookup { gift-code: gift-code })) ERR_GIFT_CODE_EXISTS)
    
    (map-set gift-cards
      { gift-id: gift-id }
      {
        gift-code: gift-code,
        gifter: tx-sender,
        recipient: recipient,
        amount: (get amount-per-gift batch),
        subscription-months: (/ (get amount-per-gift batch) u1000000),
        is-claimed: false,
        created-at: current-block,
        expires-at: expiration-block,
        claimed-at: u0,
        gift-message: gift-message,
        gift-type: "corporate"
      }
    )
    
    (map-set gift-code-lookup { gift-code: gift-code } { gift-id: gift-id })
    (var-set next-gift-id (+ gift-id u1))
    
    (ok gift-id)
  )
)

(define-public (extend-gift-expiration (gift-id uint) (additional-blocks uint))
  (let
    (
      (gift-card (unwrap! (map-get? gift-cards { gift-id: gift-id }) ERR_GIFT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get gifter gift-card)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-claimed gift-card)) ERR_GIFT_ALREADY_CLAIMED)
    
    (map-set gift-cards
      { gift-id: gift-id }
      (merge gift-card {
        expires-at: (+ (get expires-at gift-card) additional-blocks)
      })
    )
    (ok true)
  )
)

(define-public (cancel-gift (gift-id uint))
  (let
    (
      (gift-card (unwrap! (map-get? gift-cards { gift-id: gift-id }) ERR_GIFT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get gifter gift-card)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-claimed gift-card)) ERR_GIFT_ALREADY_CLAIMED)
    (asserts! (< current-block (get expires-at gift-card)) ERR_GIFT_EXPIRED)
    
    (try! (as-contract (stx-transfer? (get amount gift-card) tx-sender (get gifter gift-card))))
    
    (map-delete gift-cards { gift-id: gift-id })
    (map-delete gift-code-lookup { gift-code: (get gift-code gift-card) })
    
    (ok (get amount gift-card))
  )
)

(define-public (bulk-create-gifts (recipients (list 10 principal)) (subscription-months uint) (gift-message (string-ascii 200)))
  (let
    (
      (amount-per-gift (* subscription-months u1000000))
      (total-recipients (len recipients))
      (platform-fee (/ (* (* amount-per-gift total-recipients) (var-get platform-fee-percentage)) u100))
      (total-cost (+ (* amount-per-gift total-recipients) platform-fee))
    )
    (asserts! (<= amount-per-gift (var-get max-gift-value)) ERR_INSUFFICIENT_PAYMENT)
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (ok (map create-individual-gift recipients))
  )
)

(define-private (create-individual-gift (recipient principal))
  (let
    (
      (gift-id (var-get next-gift-id))
      (random-code (generate-gift-code gift-id))
      (current-block stacks-block-height)
      (expiration-block (+ current-block (var-get gift-expiration-blocks)))
    )
    (map-set gift-cards
      { gift-id: gift-id }
      {
        gift-code: random-code,
        gifter: tx-sender,
        recipient: (some recipient),
        amount: u1000000,
        subscription-months: u1,
        is-claimed: false,
        created-at: current-block,
        expires-at: expiration-block,
        claimed-at: u0,
        gift-message: "Bulk gift subscription",
        gift-type: "bulk"
      }
    )
    (map-set gift-code-lookup { gift-code: random-code } { gift-id: gift-id })
    (var-set next-gift-id (+ gift-id u1))
    gift-id
  )
)

(define-private (generate-gift-code (seed uint))
  (let
    (
      (hash-input (+ seed stacks-block-height))
      (code-suffix (int-to-ascii (mod hash-input u999999)))
    )
    (unwrap-panic (as-max-len? (concat "GIFT" code-suffix) u16))
  )
)

(define-private (update-user-gift-stats (user principal) (action (string-ascii 10)) (amount uint))
  (let
    (
      (current-stats (default-to
        { gifts-sent: u0, gifts-received: u0, total-gifted-amount: u0, total-received-amount: u0, last-gift-sent: u0, last-gift-received: u0 }
        (map-get? user-gift-stats { user: user })))
    )
    (map-set user-gift-stats
      { user: user }
      (if (is-eq action "sent")
        {
          gifts-sent: (+ (get gifts-sent current-stats) u1),
          gifts-received: (get gifts-received current-stats),
          total-gifted-amount: (+ (get total-gifted-amount current-stats) amount),
          total-received-amount: (get total-received-amount current-stats),
          last-gift-sent: stacks-block-height,
          last-gift-received: (get last-gift-received current-stats)
        }
        {
          gifts-sent: (get gifts-sent current-stats),
          gifts-received: (+ (get gifts-received current-stats) u1),
          total-gifted-amount: (get total-gifted-amount current-stats),
          total-received-amount: (+ (get total-received-amount current-stats) amount),
          last-gift-sent: (get last-gift-sent current-stats),
          last-gift-received: stacks-block-height
        })
    )
  )
)

(define-public (set-gift-parameters (expiration-blocks uint) (fee-percentage uint) (max-value uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set gift-expiration-blocks expiration-blocks)
    (var-set platform-fee-percentage fee-percentage)
    (var-set max-gift-value max-value)
    (ok true)
  )
)

(define-read-only (get-gift-card (gift-id uint))
  (map-get? gift-cards { gift-id: gift-id })
)

(define-read-only (get-gift-by-code (gift-code (string-ascii 16)))
  (match (map-get? gift-code-lookup { gift-code: gift-code })
    lookup
      (map-get? gift-cards { gift-id: (get gift-id lookup) })
    none
  )
)

(define-read-only (get-user-gift-stats (user principal))
  (map-get? user-gift-stats { user: user })
)

(define-read-only (get-gift-redemption-history (gift-id uint))
  (map-get? gift-redemption-history { gift-id: gift-id })
)

(define-read-only (get-corporate-batch (batch-id uint))
  (map-get? corporate-gift-batches { batch-id: batch-id })
)

(define-read-only (is-gift-valid (gift-code (string-ascii 16)))
  (match (get-gift-by-code gift-code)
    gift-card
      (ok {
        is-valid: (and (not (get is-claimed gift-card)) (> (get expires-at gift-card) stacks-block-height)),
        expires-at: (get expires-at gift-card),
        amount: (get amount gift-card),
        subscription-months: (get subscription-months gift-card)
      })
    (ok { is-valid: false, expires-at: u0, amount: u0, subscription-months: u0 })
  )
)

(define-read-only (calculate-gift-cost (subscription-months uint))
  (let
    (
      (base-amount (* subscription-months u1000000))
      (platform-fee (/ (* base-amount (var-get platform-fee-percentage)) u100))
    )
    (ok {
      base-amount: base-amount,
      platform-fee: platform-fee,
      total-cost: (+ base-amount platform-fee)
    })
  )
)

(define-read-only (get-platform-stats)
  (ok {
    total-gifts-created: (var-get total-gifts-created),
    total-gifts-claimed: (var-get total-gifts-claimed),
    gift-expiration-blocks: (var-get gift-expiration-blocks),
    platform-fee-percentage: (var-get platform-fee-percentage),
    max-gift-value: (var-get max-gift-value)
  })
)

(define-read-only (get-gift-settings)
  (ok {
    expiration-blocks: (var-get gift-expiration-blocks),
    platform-fee-percentage: (var-get platform-fee-percentage),
    max-gift-value: (var-get max-gift-value),
    next-gift-id: (var-get next-gift-id),
    next-batch-id: (var-get next-batch-id)
  })
)
