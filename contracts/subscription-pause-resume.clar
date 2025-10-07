(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u501))
(define-constant ERR_ALREADY_PAUSED (err u502))
(define-constant ERR_NOT_PAUSED (err u503))
(define-constant ERR_INVALID_PAUSE_DURATION (err u504))
(define-constant ERR_PAUSE_LIMIT_EXCEEDED (err u505))
(define-constant ERR_INSUFFICIENT_CREDIT (err u506))

(define-data-var max-pause-duration uint u4320)
(define-data-var max-pauses-per-period uint u3)
(define-data-var pause-fee-percentage uint u5)
(define-data-var minimum-active-days uint u30)

(define-map paused-subscriptions
  { subscriber: principal }
  {
    original-end-block: uint,
    pause-start-block: uint,
    pause-duration: uint,
    accumulated-credit: uint,
    refund-amount: uint,
    pause-reason: (string-ascii 100),
    auto-resume: bool
  }
)

(define-map pause-history
  { subscriber: principal, pause-id: uint }
  {
    pause-date: uint,
    resume-date: uint,
    duration: uint,
    refund-given: uint,
    reason: (string-ascii 100)
  }
)

(define-map subscriber-pause-stats
  { subscriber: principal }
  {
    total-pauses: uint,
    current-period-pauses: uint,
    last-pause-block: uint,
    total-pause-days: uint,
    period-reset-block: uint
  }
)

(define-map subscription-credits
  { subscriber: principal }
  {
    available-credits: uint,
    total-earned: uint,
    total-used: uint,
    last-updated: uint
  }
)

(define-data-var next-pause-id uint u1)

(define-public (pause-subscription (duration uint) (reason (string-ascii 100)) (auto-resume bool))
  (let
    (
      (current-block stacks-block-height)
      (pause-stats (default-to 
        { total-pauses: u0, current-period-pauses: u0, last-pause-block: u0, total-pause-days: u0, period-reset-block: current-block }
        (map-get? subscriber-pause-stats { subscriber: tx-sender })))
      (pause-id (var-get next-pause-id))
      (days-since-last (- current-block (get last-pause-block pause-stats)))
      (period-reset-needed (> days-since-last u4320))
      (current-period-pauses (if period-reset-needed u0 (get current-period-pauses pause-stats)))
    )
    (asserts! (<= duration (var-get max-pause-duration)) ERR_INVALID_PAUSE_DURATION)
    (asserts! (> duration u0) ERR_INVALID_PAUSE_DURATION)
    (asserts! (< current-period-pauses (var-get max-pauses-per-period)) ERR_PAUSE_LIMIT_EXCEEDED)
    (asserts! (is-none (map-get? paused-subscriptions { subscriber: tx-sender })) ERR_ALREADY_PAUSED)
    (asserts! (> days-since-last (var-get minimum-active-days)) ERR_PAUSE_LIMIT_EXCEEDED)
    
    (let
      (
        (refund-amount (calculate-pause-refund duration))
        (pause-fee (/ (* refund-amount (var-get pause-fee-percentage)) u100))
        (net-refund (- refund-amount pause-fee))
      )
      (map-set paused-subscriptions
        { subscriber: tx-sender }
        {
          original-end-block: (+ current-block u2628000),
          pause-start-block: current-block,
          pause-duration: duration,
          accumulated-credit: net-refund,
          refund-amount: net-refund,
          pause-reason: reason,
          auto-resume: auto-resume
        }
      )
      
      (map-set pause-history
        { subscriber: tx-sender, pause-id: pause-id }
        {
          pause-date: current-block,
          resume-date: u0,
          duration: duration,
          refund-given: net-refund,
          reason: reason
        }
      )
      
      (map-set subscriber-pause-stats
        { subscriber: tx-sender }
        {
          total-pauses: (+ (get total-pauses pause-stats) u1),
          current-period-pauses: (+ current-period-pauses u1),
          last-pause-block: current-block,
          total-pause-days: (+ (get total-pause-days pause-stats) duration),
          period-reset-block: (if period-reset-needed current-block (get period-reset-block pause-stats))
        }
      )
      
      (update-subscription-credits tx-sender net-refund)
      (var-set next-pause-id (+ pause-id u1))
      
      (if (> net-refund u0)
        (try! (as-contract (stx-transfer? net-refund tx-sender tx-sender)))
        true)
      
      (ok {
        pause-id: pause-id,
        refund-amount: net-refund,
        resume-block: (+ current-block duration)
      })
    )
  )
)

(define-public (resume-subscription)
  (let
    (
      (pause-data (unwrap! (map-get? paused-subscriptions { subscriber: tx-sender }) ERR_NOT_PAUSED))
      (current-block stacks-block-height)
      (pause-start (get pause-start-block pause-data))
      (planned-duration (get pause-duration pause-data))
      (actual-pause-duration (- current-block pause-start))
      (remaining-duration (if (> planned-duration actual-pause-duration) 
                            (- planned-duration actual-pause-duration) 
                            u0))
      (credit-to-add (if (> remaining-duration u0) 
                       (calculate-resume-credit remaining-duration) 
                       u0))
    )
    
    (map-delete paused-subscriptions { subscriber: tx-sender })
    
    (let
      (
        (pause-id (- (var-get next-pause-id) u1))
        (existing-history (unwrap! (map-get? pause-history { subscriber: tx-sender, pause-id: pause-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      )
      (map-set pause-history
        { subscriber: tx-sender, pause-id: pause-id }
        (merge existing-history { resume-date: current-block })
      )
    )
    
    (if (> credit-to-add u0)
      (update-subscription-credits tx-sender credit-to-add)
      true)
    
    (ok {
      actual-pause-duration: actual-pause-duration,
      credit-earned: credit-to-add,
      resume-block: current-block
    })
  )
)

(define-public (extend-pause (additional-duration uint))
  (let
    (
      (pause-data (unwrap! (map-get? paused-subscriptions { subscriber: tx-sender }) ERR_NOT_PAUSED))
      (current-duration (get pause-duration pause-data))
      (new-duration (+ current-duration additional-duration))
    )
    (asserts! (<= new-duration (var-get max-pause-duration)) ERR_INVALID_PAUSE_DURATION)
    
    (map-set paused-subscriptions
      { subscriber: tx-sender }
      (merge pause-data { pause-duration: new-duration })
    )
    (ok new-duration)
  )
)

(define-public (apply-credit-to-subscription (credit-amount uint))
  (let
    (
      (credits (unwrap! (map-get? subscription-credits { subscriber: tx-sender }) ERR_INSUFFICIENT_CREDIT))
      (available (get available-credits credits))
    )
    (asserts! (<= credit-amount available) ERR_INSUFFICIENT_CREDIT)
    
    (map-set subscription-credits
      { subscriber: tx-sender }
      (merge credits {
        available-credits: (- available credit-amount),
        total-used: (+ (get total-used credits) credit-amount),
        last-updated: stacks-block-height
      })
    )
    (ok credit-amount)
  )
)

(define-public (auto-resume-check (subscriber principal))
  (let
    (
      (pause-data (map-get? paused-subscriptions { subscriber: subscriber }))
    )
    (match pause-data
      data
        (if (and (get auto-resume data) 
                 (>= stacks-block-height (+ (get pause-start-block data) (get pause-duration data))))
          (begin
            (map-delete paused-subscriptions { subscriber: subscriber })
            (ok true))
          (ok false))
      (ok false)
    )
  )
)

(define-public (set-pause-parameters (max-duration uint) (max-pauses uint) (fee-percentage uint) (min-active uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set max-pause-duration max-duration)
    (var-set max-pauses-per-period max-pauses)
    (var-set pause-fee-percentage fee-percentage)
    (var-set minimum-active-days min-active)
    (ok true)
  )
)

(define-private (calculate-pause-refund (duration uint))
  (let
    (
      (daily-rate u34246)
      (refund-amount (* duration daily-rate))
    )
    refund-amount
  )
)

(define-private (calculate-resume-credit (remaining-duration uint))
  (let
    (
      (daily-rate u34246)
      (credit-amount (* remaining-duration daily-rate))
    )
    credit-amount
  )
)

(define-private (update-subscription-credits (user principal) (amount uint))
  (let
    (
      (current-credits (default-to 
        { available-credits: u0, total-earned: u0, total-used: u0, last-updated: stacks-block-height }
        (map-get? subscription-credits { subscriber: user })))
    )
    (map-set subscription-credits
      { subscriber: user }
      {
        available-credits: (+ (get available-credits current-credits) amount),
        total-earned: (+ (get total-earned current-credits) amount),
        total-used: (get total-used current-credits),
        last-updated: stacks-block-height
      }
    )
  )
)

(define-read-only (get-pause-status (subscriber principal))
  (match (map-get? paused-subscriptions { subscriber: subscriber })
    pause-data
      (ok {
        is-paused: true,
        pause-start: (get pause-start-block pause-data),
        planned-end: (+ (get pause-start-block pause-data) (get pause-duration pause-data)),
        accumulated-credit: (get accumulated-credit pause-data),
        auto-resume: (get auto-resume pause-data),
        reason: (get pause-reason pause-data)
      })
    (ok { is-paused: false, pause-start: u0, planned-end: u0, accumulated-credit: u0, auto-resume: false, reason: "" })
  )
)

(define-read-only (get-pause-stats (subscriber principal))
  (map-get? subscriber-pause-stats { subscriber: subscriber })
)

(define-read-only (get-subscription-credits (subscriber principal))
  (map-get? subscription-credits { subscriber: subscriber })
)

(define-read-only (get-pause-history (subscriber principal) (pause-id uint))
  (map-get? pause-history { subscriber: subscriber, pause-id: pause-id })
)

(define-read-only (can-pause-subscription (subscriber principal))
  (let
    (
      (pause-stats (map-get? subscriber-pause-stats { subscriber: subscriber }))
      (is-currently-paused (is-some (map-get? paused-subscriptions { subscriber: subscriber })))
    )
    (if is-currently-paused
      (ok { can-pause: false, reason: "already-paused" })
      (match pause-stats
        stats
          (let
            (
              (days-since-last (- stacks-block-height (get last-pause-block stats)))
              (current-period-pauses (get current-period-pauses stats))
            )
            (if (< days-since-last (var-get minimum-active-days))
              (ok { can-pause: false, reason: "minimum-active-not-met" })
              (if (>= current-period-pauses (var-get max-pauses-per-period))
                (ok { can-pause: false, reason: "pause-limit-exceeded" })
                (ok { can-pause: true, reason: "eligible" }))))
        (ok { can-pause: true, reason: "first-time" })
      )
    )
  )
)

(define-read-only (calculate-pause-cost (duration uint))
  (let
    (
      (refund-amount (calculate-pause-refund duration))
      (pause-fee (/ (* refund-amount (var-get pause-fee-percentage)) u100))
    )
    (ok {
      refund-amount: refund-amount,
      pause-fee: pause-fee,
      net-refund: (- refund-amount pause-fee)
    })
  )
)

(define-read-only (get-pause-settings)
  (ok {
    max-pause-duration: (var-get max-pause-duration),
    max-pauses-per-period: (var-get max-pauses-per-period),
    pause-fee-percentage: (var-get pause-fee-percentage),
    minimum-active-days: (var-get minimum-active-days)
  })
)
