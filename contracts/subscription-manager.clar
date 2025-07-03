(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u101))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INVALID_DURATION (err u104))
(define-constant ERR_SUBSCRIPTION_EXISTS (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))

(define-constant ERR_DISCOUNT_NOT_FOUND (err u201))
(define-constant ERR_INVALID_PERCENTAGE (err u202))
(define-constant ERR_REFERRAL_NOT_FOUND (err u203))
(define-constant ERR_CANNOT_REFER_SELF (err u204))
(define-constant ERR_TIER_NOT_FOUND (err u205))

(define-data-var next-discount-id uint u1)
(define-data-var referral-bonus-percentage uint u10)
(define-data-var max-discount-percentage uint u50)

(define-data-var subscription-fee uint u1000000)
(define-data-var subscription-duration uint u2628000)
(define-data-var total-subscribers uint u0)
(define-data-var contract-balance uint u0)

(define-map subscriptions
  { subscriber: principal }
  {
    start-block: uint,
    end-block: uint,
    amount-paid: uint,
    is-active: bool,
    renewal-count: uint
  }
)

(define-map subscription-history
  { subscriber: principal, renewal-id: uint }
  {
    payment-block: uint,
    amount: uint,
    duration: uint
  }
)

(define-public (set-subscription-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-fee u0) ERR_INVALID_AMOUNT)
    (var-set subscription-fee new-fee)
    (ok true)
  )
)

(define-public (set-subscription-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-duration u0) ERR_INVALID_DURATION)
    (var-set subscription-duration new-duration)
    (ok true)
  )
)

(define-public (subscribe)
  (let
    (
      (current-fee (var-get subscription-fee))
      (duration (var-get subscription-duration))
      (current-block stacks-block-height)
      (end-block (+ current-block duration))
      (existing-sub (map-get? subscriptions { subscriber: tx-sender }))
    )
    (asserts! (is-none existing-sub) ERR_SUBSCRIPTION_EXISTS)
    (try! (stx-transfer? current-fee tx-sender (as-contract tx-sender)))
    (map-set subscriptions
      { subscriber: tx-sender }
      {
        start-block: current-block,
        end-block: end-block,
        amount-paid: current-fee,
        is-active: true,
        renewal-count: u1
      }
    )
    (map-set subscription-history
      { subscriber: tx-sender, renewal-id: u1 }
      {
        payment-block: current-block,
        amount: current-fee,
        duration: duration
      }
    )
    (var-set total-subscribers (+ (var-get total-subscribers) u1))
    (var-set contract-balance (+ (var-get contract-balance) current-fee))
    (ok true)
  )
)

(define-public (renew-subscription)
  (let
    (
      (current-fee (var-get subscription-fee))
      (duration (var-get subscription-duration))
      (current-block stacks-block-height)
      (subscription (unwrap! (map-get? subscriptions { subscriber: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
      (new-end-block (+ current-block duration))
      (new-renewal-count (+ (get renewal-count subscription) u1))
    )
    (try! (stx-transfer? current-fee tx-sender (as-contract tx-sender)))
    (map-set subscriptions
      { subscriber: tx-sender }
      {
        start-block: current-block,
        end-block: new-end-block,
        amount-paid: current-fee,
        is-active: true,
        renewal-count: new-renewal-count
      }
    )
    (map-set subscription-history
      { subscriber: tx-sender, renewal-id: new-renewal-count }
      {
        payment-block: current-block,
        amount: current-fee,
        duration: duration
      }
    )
    (var-set contract-balance (+ (var-get contract-balance) current-fee))
    (ok true)
  )
)

(define-public (cancel-subscription)
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscriber: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (map-set subscriptions
      { subscriber: tx-sender }
      (merge subscription { is-active: false })
    )
    (var-set total-subscribers (- (var-get total-subscribers) u1))
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get contract-balance)) ERR_INSUFFICIENT_PAYMENT)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok true)
  )
)

(define-public (force-expire-subscription (subscriber principal))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscriber: subscriber }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set subscriptions
      { subscriber: subscriber }
      (merge subscription { is-active: false, end-block: stacks-block-height })
    )
    (var-set total-subscribers (- (var-get total-subscribers) u1))
    (ok true)
  )
)

(define-read-only (get-subscription (subscriber principal))
  (map-get? subscriptions { subscriber: subscriber })
)

(define-read-only (is-subscription-active (subscriber principal))
  (match (map-get? subscriptions { subscriber: subscriber })
    subscription
      (and
        (get is-active subscription)
        (>= (get end-block subscription) stacks-block-height)
      )
    false
  )
)

(define-read-only (get-subscription-status (subscriber principal))
  (match (map-get? subscriptions { subscriber: subscriber })
    subscription
      (let
        (
          (is-expired (< (get end-block subscription) stacks-block-height))
          (is-cancelled (not (get is-active subscription)))
        )
        (ok {
          subscription: subscription,
          is-active: (and (get is-active subscription) (not is-expired)),
          is-expired: is-expired,
          is-cancelled: is-cancelled,
          blocks-remaining: (if is-expired u0 (- (get end-block subscription) stacks-block-height))
        })
      )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-read-only (get-subscription-history (subscriber principal) (renewal-id uint))
  (map-get? subscription-history { subscriber: subscriber, renewal-id: renewal-id })
)

(define-read-only (get-contract-info)
  (ok {
    subscription-fee: (var-get subscription-fee),
    subscription-duration: (var-get subscription-duration),
    total-subscribers: (var-get total-subscribers),
    contract-balance: (var-get contract-balance),
    contract-owner: CONTRACT_OWNER
  })
)

(define-read-only (get-subscription-fee)
  (var-get subscription-fee)
)

(define-read-only (get-subscription-duration)
  (var-get subscription-duration)
)

(define-read-only (get-total-subscribers)
  (var-get total-subscribers)
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (time-until-expiry (subscriber principal))
  (match (map-get? subscriptions { subscriber: subscriber })
    subscription
      (if (>= (get end-block subscription) stacks-block-height)
        (ok (- (get end-block subscription) stacks-block-height))
        (ok u0)
      )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-read-only (bulk-check-subscriptions (subscribers (list 10 principal)))
  (map is-subscription-active subscribers)
)



(define-map discount-codes
  { discount-id: uint }
  {
    code: (string-ascii 20),
    percentage: uint,
    max-uses: uint,
    current-uses: uint,
    creator: principal,
    is-active: bool,
    expires-at-block: uint
  }
)

(define-map code-lookup
  { code: (string-ascii 20) }
  { discount-id: uint }
)

(define-map user-discounts
  { user: principal, discount-id: uint }
  { used-at-block: uint }
)

(define-map referral-links
  { referrer: principal }
  {
    total-referrals: uint,
    total-earned: uint,
    is-active: bool
  }
)

(define-map referral-uses
  { referee: principal }
  { referrer: principal, bonus-earned: uint }
)

(define-map loyalty-tiers
  { tier-level: uint }
  {
    min-renewals: uint,
    discount-percentage: uint,
    tier-name: (string-ascii 30)
  }
)

(define-map user-loyalty
  { user: principal }
  {
    current-tier: uint,
    total-renewals: uint,
    tier-discount: uint
  }
)

(define-public (create-discount-code (code (string-ascii 20)) (percentage uint) (max-uses uint) (expires-at-block uint))
  (let
    (
      (discount-id (var-get next-discount-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= percentage (var-get max-discount-percentage)) ERR_INVALID_PERCENTAGE)
    (map-set discount-codes
      { discount-id: discount-id }
      {
        code: code,
        percentage: percentage,
        max-uses: max-uses,
        current-uses: u0,
        creator: tx-sender,
        is-active: true,
        expires-at-block: expires-at-block
      }
    )
    (map-set code-lookup { code: code } { discount-id: discount-id })
    (var-set next-discount-id (+ discount-id u1))
    (ok discount-id)
  )
)

(define-public (use-discount-code (code (string-ascii 20)) (user principal))
  (let
    (
      (discount-lookup (unwrap! (map-get? code-lookup { code: code }) ERR_DISCOUNT_NOT_FOUND))
      (discount-id (get discount-id discount-lookup))
      (discount (unwrap! (map-get? discount-codes { discount-id: discount-id }) ERR_DISCOUNT_NOT_FOUND))
    )
    (asserts! (get is-active discount) ERR_DISCOUNT_NOT_FOUND)
    (asserts! (< (get current-uses discount) (get max-uses discount)) ERR_DISCOUNT_NOT_FOUND)
    (asserts! (> (get expires-at-block discount) stacks-block-height) ERR_DISCOUNT_NOT_FOUND)
    (map-set discount-codes
      { discount-id: discount-id }
      (merge discount { current-uses: (+ (get current-uses discount) u1) })
    )
    (map-set user-discounts
      { user: user, discount-id: discount-id }
      { used-at-block: stacks-block-height }
    )
    (ok (get percentage discount))
  )
)

(define-public (create-referral-link)
  (begin
    (map-set referral-links
      { referrer: tx-sender }
      {
        total-referrals: u0,
        total-earned: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (use-referral (referrer principal) (referee principal))
  (let
    (
      (referral-data (unwrap! (map-get? referral-links { referrer: referrer }) ERR_REFERRAL_NOT_FOUND))
      (bonus-percentage (var-get referral-bonus-percentage))
    )
    (asserts! (not (is-eq referrer referee)) ERR_CANNOT_REFER_SELF)
    (asserts! (get is-active referral-data) ERR_REFERRAL_NOT_FOUND)
    (map-set referral-links
      { referrer: referrer }
      (merge referral-data { 
        total-referrals: (+ (get total-referrals referral-data) u1),
        total-earned: (+ (get total-earned referral-data) bonus-percentage)
      })
    )
    (map-set referral-uses
      { referee: referee }
      { referrer: referrer, bonus-earned: bonus-percentage }
    )
    (ok bonus-percentage)
  )
)

(define-public (setup-loyalty-tier (tier-level uint) (min-renewals uint) (discount-percentage uint) (tier-name (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= discount-percentage (var-get max-discount-percentage)) ERR_INVALID_PERCENTAGE)
    (map-set loyalty-tiers
      { tier-level: tier-level }
      {
        min-renewals: min-renewals,
        discount-percentage: discount-percentage,
        tier-name: tier-name
      }
    )
    (ok true)
  )
)

(define-public (update-user-loyalty (user principal) (renewal-count uint))
  (let
    (
      (current-loyalty (default-to { current-tier: u0, total-renewals: u0, tier-discount: u0 }
                                   (map-get? user-loyalty { user: user })))
      (new-tier (calculate-tier renewal-count))
      (tier-data (map-get? loyalty-tiers { tier-level: new-tier }))
    )
    (map-set user-loyalty
      { user: user }
      {
        current-tier: new-tier,
        total-renewals: renewal-count,
        tier-discount: (match tier-data
                        tier (get discount-percentage tier)
                        u0)
      }
    )
    (ok new-tier)
  )
)

(define-private (calculate-tier (renewals uint))
  (if (>= renewals u20) u4
    (if (>= renewals u10) u3
      (if (>= renewals u5) u2
        (if (>= renewals u1) u1 u0)))))

(define-public (calculate-final-discount (user principal) (base-amount uint))
  (let
    (
      (loyalty-data (map-get? user-loyalty { user: user }))
      (loyalty-discount (match loyalty-data
                         data (get tier-discount data)
                         u0))
      (discount-amount (/ (* base-amount loyalty-discount) u100))
    )
    (ok (- base-amount discount-amount))
  )
)

(define-public (deactivate-discount (discount-id uint))
  (let
    (
      (discount (unwrap! (map-get? discount-codes { discount-id: discount-id }) ERR_DISCOUNT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set discount-codes
      { discount-id: discount-id }
      (merge discount { is-active: false })
    )
    (ok true)
  )
)

(define-read-only (get-discount-by-code (code (string-ascii 20)))
  (match (map-get? code-lookup { code: code })
    lookup
      (map-get? discount-codes { discount-id: (get discount-id lookup) })
    none
  )
)

(define-read-only (get-user-loyalty (user principal))
  (map-get? user-loyalty { user: user })
)

(define-read-only (get-referral-data (referrer principal))
  (map-get? referral-links { referrer: referrer })
)

(define-read-only (get-tier-info (tier-level uint))
  (map-get? loyalty-tiers { tier-level: tier-level })
)

(define-read-only (has-used-discount (user principal) (discount-id uint))
  (is-some (map-get? user-discounts { user: user, discount-id: discount-id }))
)

(define-read-only (get-referral-bonus-percentage)
  (var-get referral-bonus-percentage)
)

(define-read-only (get-max-discount-percentage)
  (var-get max-discount-percentage)
)
