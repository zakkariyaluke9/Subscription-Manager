(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u700))
(define-constant ERR_TIER_NOT_FOUND (err u701))
(define-constant ERR_INVALID_TIER (err u702))
(define-constant ERR_SAME_TIER (err u703))
(define-constant ERR_TIER_EXISTS (err u704))
(define-constant ERR_NO_SUBSCRIPTION (err u705))
(define-constant ERR_INVALID_PRICE (err u706))
(define-constant ERR_FEATURE_NOT_FOUND (err u707))

(define-data-var next-tier-id uint u1)
(define-data-var total-tier-changes uint u0)
(define-data-var upgrade-fee-percentage uint u0)
(define-data-var downgrade-fee-percentage uint u10)

(define-map subscription-tiers
  { tier-id: uint }
  {
    tier-name: (string-ascii 30),
    tier-slug: (string-ascii 20),
    monthly-price: uint,
    annual-price: uint,
    max-users: uint,
    storage-gb: uint,
    tier-level: uint,
    is-active: bool,
    created-at: uint,
    features-count: uint
  }
)

(define-map tier-slug-lookup
  { tier-slug: (string-ascii 20) }
  { tier-id: uint }
)

(define-map tier-features
  { tier-id: uint, feature-id: uint }
  {
    feature-name: (string-ascii 50),
    feature-value: (string-ascii 100),
    is-enabled: bool
  }
)

(define-map user-subscriptions
  { user: principal }
  {
    current-tier-id: uint,
    billing-cycle: (string-ascii 10),
    subscription-start: uint,
    subscription-end: uint,
    auto-renew: bool,
    total-paid: uint,
    upgrade-count: uint,
    downgrade-count: uint
  }
)

(define-map tier-change-history
  { user: principal, change-id: uint }
  {
    from-tier-id: uint,
    to-tier-id: uint,
    change-type: (string-ascii 20),
    change-date: uint,
    prorated-credit: uint,
    additional-payment: uint,
    reason: (string-ascii 100)
  }
)

(define-map tier-analytics
  { tier-id: uint }
  {
    total-subscribers: uint,
    monthly-subscribers: uint,
    annual-subscribers: uint,
    total-revenue: uint,
    upgrades-to: uint,
    downgrades-from: uint,
    churn-count: uint
  }
)

(define-map feature-usage
  { user: principal, feature-id: uint }
  {
    usage-count: uint,
    last-used: uint,
    quota-remaining: uint
  }
)

(define-data-var next-change-id uint u1)

(define-public (create-tier (tier-name (string-ascii 30)) (tier-slug (string-ascii 20)) (monthly-price uint) (annual-price uint) (max-users uint) (storage-gb uint) (tier-level uint))
  (let
    (
      (tier-id (var-get next-tier-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> monthly-price u0) ERR_INVALID_PRICE)
    (asserts! (is-none (map-get? tier-slug-lookup { tier-slug: tier-slug })) ERR_TIER_EXISTS)
    
    (map-set subscription-tiers
      { tier-id: tier-id }
      {
        tier-name: tier-name,
        tier-slug: tier-slug,
        monthly-price: monthly-price,
        annual-price: annual-price,
        max-users: max-users,
        storage-gb: storage-gb,
        tier-level: tier-level,
        is-active: true,
        created-at: stacks-block-height,
        features-count: u0
      }
    )
    
    (map-set tier-slug-lookup { tier-slug: tier-slug } { tier-id: tier-id })
    
    (map-set tier-analytics
      { tier-id: tier-id }
      {
        total-subscribers: u0,
        monthly-subscribers: u0,
        annual-subscribers: u0,
        total-revenue: u0,
        upgrades-to: u0,
        downgrades-from: u0,
        churn-count: u0
      }
    )
    
    (var-set next-tier-id (+ tier-id u1))
    (ok tier-id)
  )
)

(define-public (subscribe-to-tier (tier-id uint) (billing-cycle (string-ascii 10)))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_TIER_NOT_FOUND))
      (price (if (is-eq billing-cycle "monthly") (get monthly-price tier) (get annual-price tier)))
      (duration (if (is-eq billing-cycle "monthly") u4320 u52560))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active tier) ERR_INVALID_TIER)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    (map-set user-subscriptions
      { user: tx-sender }
      {
        current-tier-id: tier-id,
        billing-cycle: billing-cycle,
        subscription-start: current-block,
        subscription-end: (+ current-block duration),
        auto-renew: true,
        total-paid: price,
        upgrade-count: u0,
        downgrade-count: u0
      }
    )
    
    (update-tier-analytics tier-id billing-cycle "subscribe" price)
    (ok true)
  )
)

(define-public (upgrade-tier (new-tier-id uint))
  (let
    (
      (user-sub (unwrap! (map-get? user-subscriptions { user: tx-sender }) ERR_NO_SUBSCRIPTION))
      (current-tier-id (get current-tier-id user-sub))
      (current-tier (unwrap! (map-get? subscription-tiers { tier-id: current-tier-id }) ERR_TIER_NOT_FOUND))
      (new-tier (unwrap! (map-get? subscription-tiers { tier-id: new-tier-id }) ERR_TIER_NOT_FOUND))
      (current-block stacks-block-height)
      (blocks-remaining (- (get subscription-end user-sub) current-block))
      (billing-cycle (get billing-cycle user-sub))
      (current-price (if (is-eq billing-cycle "monthly") (get monthly-price current-tier) (get annual-price current-tier)))
      (new-price (if (is-eq billing-cycle "monthly") (get monthly-price new-tier) (get annual-price new-tier)))
      (cycle-duration (if (is-eq billing-cycle "monthly") u4320 u52560))
      (prorated-credit (/ (* current-price blocks-remaining) cycle-duration))
      (additional-payment (if (> new-price prorated-credit) (- new-price prorated-credit) u0))
      (change-id (var-get next-change-id))
    )
    (asserts! (not (is-eq current-tier-id new-tier-id)) ERR_SAME_TIER)
    (asserts! (> (get tier-level new-tier) (get tier-level current-tier)) ERR_INVALID_TIER)
    (asserts! (get is-active new-tier) ERR_INVALID_TIER)
    
    (if (> additional-payment u0)
      (try! (stx-transfer? additional-payment tx-sender (as-contract tx-sender)))
      true)
    
    (map-set user-subscriptions
      { user: tx-sender }
      (merge user-sub {
        current-tier-id: new-tier-id,
        total-paid: (+ (get total-paid user-sub) additional-payment),
        upgrade-count: (+ (get upgrade-count user-sub) u1)
      })
    )
    
    (map-set tier-change-history
      { user: tx-sender, change-id: change-id }
      {
        from-tier-id: current-tier-id,
        to-tier-id: new-tier-id,
        change-type: "upgrade",
        change-date: current-block,
        prorated-credit: prorated-credit,
        additional-payment: additional-payment,
        reason: "user-initiated-upgrade"
      }
    )
    
    (update-tier-analytics current-tier-id billing-cycle "downgrade-from" u0)
    (update-tier-analytics new-tier-id billing-cycle "upgrade-to" additional-payment)
    (var-set next-change-id (+ change-id u1))
    (var-set total-tier-changes (+ (var-get total-tier-changes) u1))
    
    (ok {
      prorated-credit: prorated-credit,
      additional-payment: additional-payment,
      new-tier-id: new-tier-id
    })
  )
)

(define-public (downgrade-tier (new-tier-id uint))
  (let
    (
      (user-sub (unwrap! (map-get? user-subscriptions { user: tx-sender }) ERR_NO_SUBSCRIPTION))
      (current-tier-id (get current-tier-id user-sub))
      (current-tier (unwrap! (map-get? subscription-tiers { tier-id: current-tier-id }) ERR_TIER_NOT_FOUND))
      (new-tier (unwrap! (map-get? subscription-tiers { tier-id: new-tier-id }) ERR_TIER_NOT_FOUND))
      (current-block stacks-block-height)
      (blocks-remaining (- (get subscription-end user-sub) current-block))
      (billing-cycle (get billing-cycle user-sub))
      (current-price (if (is-eq billing-cycle "monthly") (get monthly-price current-tier) (get annual-price current-tier)))
      (new-price (if (is-eq billing-cycle "monthly") (get monthly-price new-tier) (get annual-price new-tier)))
      (cycle-duration (if (is-eq billing-cycle "monthly") u4320 u52560))
      (prorated-credit (/ (* current-price blocks-remaining) cycle-duration))
      (downgrade-fee (/ (* prorated-credit (var-get downgrade-fee-percentage)) u100))
      (net-credit (- prorated-credit downgrade-fee))
      (change-id (var-get next-change-id))
    )
    (asserts! (not (is-eq current-tier-id new-tier-id)) ERR_SAME_TIER)
    (asserts! (< (get tier-level new-tier) (get tier-level current-tier)) ERR_INVALID_TIER)
    (asserts! (get is-active new-tier) ERR_INVALID_TIER)
    
    (map-set user-subscriptions
      { user: tx-sender }
      (merge user-sub {
        current-tier-id: new-tier-id,
        downgrade-count: (+ (get downgrade-count user-sub) u1)
      })
    )
    
    (map-set tier-change-history
      { user: tx-sender, change-id: change-id }
      {
        from-tier-id: current-tier-id,
        to-tier-id: new-tier-id,
        change-type: "downgrade",
        change-date: current-block,
        prorated-credit: net-credit,
        additional-payment: u0,
        reason: "user-initiated-downgrade"
      }
    )
    
    (update-tier-analytics current-tier-id billing-cycle "downgrade-from" u0)
    (update-tier-analytics new-tier-id billing-cycle "upgrade-to" u0)
    (var-set next-change-id (+ change-id u1))
    (var-set total-tier-changes (+ (var-get total-tier-changes) u1))
    
    (ok {
      prorated-credit: net-credit,
      downgrade-fee: downgrade-fee,
      new-tier-id: new-tier-id
    })
  )
)

(define-public (add-tier-feature (tier-id uint) (feature-id uint) (feature-name (string-ascii 50)) (feature-value (string-ascii 100)))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_TIER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set tier-features
      { tier-id: tier-id, feature-id: feature-id }
      {
        feature-name: feature-name,
        feature-value: feature-value,
        is-enabled: true
      }
    )
    
    (map-set subscription-tiers
      { tier-id: tier-id }
      (merge tier {
        features-count: (+ (get features-count tier) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (track-feature-usage (feature-id uint) (quota-used uint))
  (let
    (
      (user-sub (unwrap! (map-get? user-subscriptions { user: tx-sender }) ERR_NO_SUBSCRIPTION))
      (current-usage (default-to
        { usage-count: u0, last-used: u0, quota-remaining: u1000000 }
        (map-get? feature-usage { user: tx-sender, feature-id: feature-id })))
    )
    (map-set feature-usage
      { user: tx-sender, feature-id: feature-id }
      {
        usage-count: (+ (get usage-count current-usage) u1),
        last-used: stacks-block-height,
        quota-remaining: (- (get quota-remaining current-usage) quota-used)
      }
    )
    (ok true)
  )
)

(define-public (switch-billing-cycle (new-cycle (string-ascii 10)))
  (let
    (
      (user-sub (unwrap! (map-get? user-subscriptions { user: tx-sender }) ERR_NO_SUBSCRIPTION))
      (tier-id (get current-tier-id user-sub))
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_TIER_NOT_FOUND))
      (current-cycle (get billing-cycle user-sub))
      (new-price (if (is-eq new-cycle "monthly") (get monthly-price tier) (get annual-price tier)))
      (current-block stacks-block-height)
      (blocks-remaining (- (get subscription-end user-sub) current-block))
      (new-duration (if (is-eq new-cycle "monthly") u4320 u52560))
    )
    (asserts! (not (is-eq current-cycle new-cycle)) ERR_SAME_TIER)
    
    (try! (stx-transfer? new-price tx-sender (as-contract tx-sender)))
    
    (map-set user-subscriptions
      { user: tx-sender }
      (merge user-sub {
        billing-cycle: new-cycle,
        subscription-end: (+ current-block new-duration),
        total-paid: (+ (get total-paid user-sub) new-price)
      })
    )
    
    (ok true)
  )
)

(define-public (update-tier-pricing (tier-id uint) (monthly-price uint) (annual-price uint))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_TIER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> monthly-price u0) ERR_INVALID_PRICE)
    
    (map-set subscription-tiers
      { tier-id: tier-id }
      (merge tier {
        monthly-price: monthly-price,
        annual-price: annual-price
      })
    )
    (ok true)
  )
)

(define-public (toggle-tier-status (tier-id uint) (is-active bool))
  (let
    (
      (tier (unwrap! (map-get? subscription-tiers { tier-id: tier-id }) ERR_TIER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set subscription-tiers
      { tier-id: tier-id }
      (merge tier { is-active: is-active })
    )
    (ok true)
  )
)

(define-private (update-tier-analytics (tier-id uint) (billing-cycle (string-ascii 10)) (action (string-ascii 20)) (revenue uint))
  (let
    (
      (analytics (unwrap-panic (map-get? tier-analytics { tier-id: tier-id })))
    )
    (map-set tier-analytics
      { tier-id: tier-id }
      (if (is-eq action "subscribe")
        {
          total-subscribers: (+ (get total-subscribers analytics) u1),
          monthly-subscribers: (if (is-eq billing-cycle "monthly") (+ (get monthly-subscribers analytics) u1) (get monthly-subscribers analytics)),
          annual-subscribers: (if (is-eq billing-cycle "annual") (+ (get annual-subscribers analytics) u1) (get annual-subscribers analytics)),
          total-revenue: (+ (get total-revenue analytics) revenue),
          upgrades-to: (get upgrades-to analytics),
          downgrades-from: (get downgrades-from analytics),
          churn-count: (get churn-count analytics)
        }
        (if (is-eq action "upgrade-to")
          (merge analytics {
            total-subscribers: (+ (get total-subscribers analytics) u1),
            upgrades-to: (+ (get upgrades-to analytics) u1),
            total-revenue: (+ (get total-revenue analytics) revenue)
          })
          (if (is-eq action "downgrade-from")
            (merge analytics {
              total-subscribers: (- (get total-subscribers analytics) u1),
              downgrades-from: (+ (get downgrades-from analytics) u1)
            })
            analytics))))
  )
)

(define-read-only (get-tier (tier-id uint))
  (map-get? subscription-tiers { tier-id: tier-id })
)

(define-read-only (get-tier-by-slug (tier-slug (string-ascii 20)))
  (match (map-get? tier-slug-lookup { tier-slug: tier-slug })
    lookup
      (map-get? subscription-tiers { tier-id: (get tier-id lookup) })
    none
  )
)

(define-read-only (get-user-subscription (user principal))
  (map-get? user-subscriptions { user: user })
)

(define-read-only (get-tier-feature (tier-id uint) (feature-id uint))
  (map-get? tier-features { tier-id: tier-id, feature-id: feature-id })
)

(define-read-only (get-tier-analytics (tier-id uint))
  (map-get? tier-analytics { tier-id: tier-id })
)

(define-read-only (get-tier-change-history (user principal) (change-id uint))
  (map-get? tier-change-history { user: user, change-id: change-id })
)

(define-read-only (get-feature-usage (user principal) (feature-id uint))
  (map-get? feature-usage { user: user, feature-id: feature-id })
)

(define-read-only (has-feature-access (user principal) (feature-id uint))
  (match (map-get? user-subscriptions { user: user })
    user-sub
      (let
        (
          (tier-id (get current-tier-id user-sub))
          (feature (map-get? tier-features { tier-id: tier-id, feature-id: feature-id }))
        )
        (match feature
          feat (ok (get is-enabled feat))
          (ok false)))
    (ok false)
  )
)

(define-read-only (calculate-upgrade-cost (user principal) (new-tier-id uint))
  (match (map-get? user-subscriptions { user: user })
    user-sub
      (let
        (
          (current-tier-id (get current-tier-id user-sub))
          (current-tier (unwrap! (map-get? subscription-tiers { tier-id: current-tier-id }) ERR_TIER_NOT_FOUND))
          (new-tier (unwrap! (map-get? subscription-tiers { tier-id: new-tier-id }) ERR_TIER_NOT_FOUND))
          (current-block stacks-block-height)
          (blocks-remaining (- (get subscription-end user-sub) current-block))
          (billing-cycle (get billing-cycle user-sub))
          (current-price (if (is-eq billing-cycle "monthly") (get monthly-price current-tier) (get annual-price current-tier)))
          (new-price (if (is-eq billing-cycle "monthly") (get monthly-price new-tier) (get annual-price new-tier)))
          (cycle-duration (if (is-eq billing-cycle "monthly") u4320 u52560))
          (prorated-credit (/ (* current-price blocks-remaining) cycle-duration))
          (additional-payment (if (> new-price prorated-credit) (- new-price prorated-credit) u0))
        )
        (ok {
          current-tier-id: current-tier-id,
          new-tier-id: new-tier-id,
          prorated-credit: prorated-credit,
          additional-payment: additional-payment,
          blocks-remaining: blocks-remaining
        }))
    ERR_NO_SUBSCRIPTION
  )
)

(define-read-only (get-platform-stats)
  (ok {
    total-tiers: (- (var-get next-tier-id) u1),
    total-tier-changes: (var-get total-tier-changes),
    upgrade-fee-percentage: (var-get upgrade-fee-percentage),
    downgrade-fee-percentage: (var-get downgrade-fee-percentage)
  })
)
