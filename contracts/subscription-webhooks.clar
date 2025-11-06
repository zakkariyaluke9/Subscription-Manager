(define-constant ERR_WEBHOOK_EXISTS (err u800))
(define-constant ERR_WEBHOOK_NOT_FOUND (err u801))

(define-data-var total-webhooks uint u0)
(define-data-var active-webhooks uint u0)
(define-data-var total-preference-updates uint u0)

(define-map webhooks
  { owner: principal }
  {
    url: (string-ascii 200),
    secret: (string-ascii 64),
    is-active: bool,
    created-at: uint,
    updated-at: uint
  }
)

(define-map event-prefs
  { owner: principal }
  {
    events: (list 10 (string-ascii 20)),
    updated-at: uint
  }
)

(define-public (create-webhook (url (string-ascii 200)) (secret (string-ascii 64)))
  (let
    (
      (existing (map-get? webhooks { owner: tx-sender }))
    )
    (asserts! (is-none existing) ERR_WEBHOOK_EXISTS)
    (map-set webhooks
      { owner: tx-sender }
      {
        url: url,
        secret: secret,
        is-active: true,
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      }
    )
    (var-set total-webhooks (+ (var-get total-webhooks) u1))
    (var-set active-webhooks (+ (var-get active-webhooks) u1))
    (ok true)
  )
)

(define-public (update-webhook (url (string-ascii 200)) (secret (string-ascii 64)) (is-active bool))
  (let
    (
      (existing (unwrap! (map-get? webhooks { owner: tx-sender }) ERR_WEBHOOK_NOT_FOUND))
      (was-active (get is-active existing))
    )
    (map-set webhooks
      { owner: tx-sender }
      (merge existing {
        url: url,
        secret: secret,
        is-active: is-active,
        updated-at: stacks-block-height
      })
    )
    (if (and (not was-active) is-active)
      (var-set active-webhooks (+ (var-get active-webhooks) u1))
      (if (and was-active (not is-active))
        (var-set active-webhooks (- (var-get active-webhooks) u1))
        (var-set active-webhooks (var-get active-webhooks))))
    (ok true)
  )
)

(define-public (deactivate-webhook)
  (let
    (
      (existing (unwrap! (map-get? webhooks { owner: tx-sender }) ERR_WEBHOOK_NOT_FOUND))
    )
    (if (get is-active existing)
      (begin
        (map-set webhooks
          { owner: tx-sender }
          (merge existing { is-active: false, updated-at: stacks-block-height })
        )
        (var-set active-webhooks (- (var-get active-webhooks) u1))
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (set-event-preferences (events (list 10 (string-ascii 20))))
  (begin
    (map-set event-prefs
      { owner: tx-sender }
      { events: events, updated-at: stacks-block-height }
    )
    (var-set total-preference-updates (+ (var-get total-preference-updates) u1))
    (ok true)
  )
)

(define-read-only (get-webhook (owner principal))
  (map-get? webhooks { owner: owner })
)

(define-read-only (get-event-preferences (owner principal))
  (map-get? event-prefs { owner: owner })
)

(define-read-only (get-webhook-stats)
  (ok {
    total-webhooks: (var-get total-webhooks),
    active-webhooks: (var-get active-webhooks),
    total-preference-updates: (var-get total-preference-updates)
  })
)

