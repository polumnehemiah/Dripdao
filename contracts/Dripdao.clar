(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STREAM-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-STREAM-ALREADY-EXISTS (err u103))
(define-constant ERR-STREAM-ENDED (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))

(define-data-var dao-admin principal tx-sender)
(define-data-var treasury-balance uint u0)
(define-data-var total-streams uint u0)

(define-map salary-streams 
    { stream-id: uint }
    {
        recipient: principal,
        amount-per-block: uint,
        total-amount: uint,
        start-height: uint,
        end-height: uint,
        claimed-amount: uint,
        is-active: bool
    }
)

(define-read-only (get-dao-admin)
    (var-get dao-admin)
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (get-stream (stream-id uint))
    (map-get? salary-streams { stream-id: stream-id })
)

(define-read-only (get-claimable-amount (stream-id uint))
    (let (
        (stream (unwrap! (get-stream stream-id) (err u0)))
        (current-height stacks-block-height)
    )
        (ok (if (and (get is-active stream) (>= current-height (get start-height stream)))
            (let (
                (effective-height (if (> current-height (get end-height stream))
                    (get end-height stream)
                    current-height
                ))
                (blocks-passed (- effective-height (get start-height stream)))
                (total-claimable (* blocks-passed (get amount-per-block stream)))
                (already-claimed (get claimed-amount stream))
            )
                (- total-claimable already-claimed)
            )
            u0
        ))
    )
)

(define-public (set-dao-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (ok (var-set dao-admin new-admin))
    )
)

(define-public (fund-treasury (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) amount))
        (ok true)
    )
)

(define-public (create-salary-stream (recipient principal) (monthly-amount uint) (duration-blocks uint))
    (let (
        (stream-id (var-get total-streams))
        (start-height stacks-block-height)
        (end-height (+ start-height duration-blocks))
        (amount-per-block (/ monthly-amount duration-blocks))
        (total-amount (* amount-per-block duration-blocks))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> monthly-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= (var-get treasury-balance) total-amount) ERR-INSUFFICIENT-BALANCE)
        
        (map-set salary-streams
            { stream-id: stream-id }
            {
                recipient: recipient,
                amount-per-block: amount-per-block,
                total-amount: total-amount,
                start-height: start-height,
                end-height: end-height,
                claimed-amount: u0,
                is-active: true
            }
        )
        
        (var-set treasury-balance (- (var-get treasury-balance) total-amount))
        (var-set total-streams (+ stream-id u1))
        (ok stream-id)
    )
)

(define-public (claim-salary (stream-id uint))
    (let (
        (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
        (claimable (unwrap! (get-claimable-amount stream-id) ERR-STREAM-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get recipient stream)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (> claimable u0) ERR-INVALID-AMOUNT)
        
        (try! (as-contract (stx-transfer? claimable tx-sender (get recipient stream))))
        
        (map-set salary-streams
            { stream-id: stream-id }
            (merge stream { 
                claimed-amount: (+ (get claimed-amount stream) claimable),
                is-active: (< stacks-block-height (get end-height stream))
            })
        )
        (ok claimable)
    )
)

(define-public (cancel-stream (stream-id uint))
    (let (
        (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
        (claimable (unwrap! (get-claimable-amount stream-id) ERR-STREAM-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        
        (if (> claimable u0)
            (try! (as-contract (stx-transfer? claimable tx-sender (get recipient stream))))
            true
        )
        
        (let (
            (remaining (- (get total-amount stream) (+ (get claimed-amount stream) claimable)))
        )
            (var-set treasury-balance (+ (var-get treasury-balance) remaining))
            (map-set salary-streams
                { stream-id: stream-id }
                (merge stream { 
                    claimed-amount: (+ (get claimed-amount stream) claimable),
                    is-active: false
                })
            )
            (ok true)
        )
    )
)