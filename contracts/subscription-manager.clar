(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u101))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INVALID_DURATION (err u104))
(define-constant ERR_SUBSCRIPTION_EXISTS (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))

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
