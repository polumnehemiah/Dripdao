(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STREAM-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-STREAM-ALREADY-EXISTS (err u103))
(define-constant ERR-STREAM-ENDED (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u106))
(define-constant ERR-PROPOSAL-ENDED (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u109))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u110))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u111))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u112))
(define-constant ERR-DELIVERABLE-NOT-FOUND (err u113))
(define-constant ERR-DELIVERABLE-ALREADY-COMPLETED (err u114))
(define-constant ERR-INVALID-SCORE (err u115))
(define-constant ERR-PERFORMANCE-PERIOD-NOT-ENDED (err u116))
(define-constant ERR-MULTIPLIER-ALREADY-APPLIED (err u117))

(define-data-var dao-admin principal tx-sender)
(define-data-var treasury-balance uint u0)
(define-data-var total-streams uint u0)
(define-data-var total-proposals uint u0)
(define-data-var voting-period-blocks uint u1440)
(define-data-var quorum-percentage uint u20)
(define-data-var min-voting-power uint u1000)
(define-data-var total-deliverables uint u0)
(define-data-var performance-evaluation-period uint u4320)

(define-map salary-streams 
    { stream-id: uint }
    {
        recipient: principal,
        amount-per-block: uint,
        total-amount: uint,
        start-height: uint,
        end-height: uint,
        claimed-amount: uint,
        is-active: bool,
        performance-multiplier: uint,
        last-evaluation-height: uint,
        multiplier-applied: bool
    }
)

(define-map proposals
    { proposal-id: uint }
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 20),
        target-principal: (optional principal),
        target-amount: (optional uint),
        target-blocks: (optional uint),
        start-height: uint,
        end-height: uint,
        votes-for: uint,
        votes-against: uint,
        total-votes: uint,
        executed: bool,
        passed: bool
    }
)

(define-map votes
    { proposal-id: uint, voter: principal }
    {
        vote-power: uint,
        vote-for: bool,
        vote-height: uint
    }
)

(define-map voting-power
    { holder: principal }
    {
        power: uint,
        delegated-to: (optional principal),
        last-update: uint
    }
)

(define-map deliverables
    { deliverable-id: uint }
    {
        assignee: principal,
        stream-id: uint,
        title: (string-ascii 100),
        description: (string-ascii 300),
        due-height: uint,
        completed: bool,
        completion-height: (optional uint),
        performance-score: (optional uint)
    }
)

(define-map performance-reviews
    { stream-id: uint, review-period: uint }
    {
        reviewer: principal,
        total-deliverables: uint,
        completed-deliverables: uint,
        average-score: uint,
        performance-multiplier: uint,
        review-height: uint,
        finalized: bool
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

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (holder principal))
    (let (
        (power-data (map-get? voting-power { holder: holder }))
    )
        (match power-data
            power-record (ok (get power power-record))
            (ok u0)
        )
    )
)

(define-read-only (get-deliverable (deliverable-id uint))
    (map-get? deliverables { deliverable-id: deliverable-id })
)

(define-read-only (get-performance-review (stream-id uint) (review-period uint))
    (map-get? performance-reviews { stream-id: stream-id, review-period: review-period })
)

(define-read-only (get-stream-performance-multiplier (stream-id uint))
    (let (
        (stream (map-get? salary-streams { stream-id: stream-id }))
    )
        (match stream
            stream-data (ok (get performance-multiplier stream-data))
            (err ERR-STREAM-NOT-FOUND)
        )
    )
)

(define-read-only (get-effective-voting-power (holder principal))
    (let (
        (power-data (map-get? voting-power { holder: holder }))
    )
        (match power-data
            power-record (match (get delegated-to power-record)
                delegated-principal (ok u0)
                (ok (get power power-record))
            )
            (ok u0)
        )
    )
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

(define-public (grant-voting-power (holder principal) (power uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> power u0) ERR-INVALID-AMOUNT)
        (map-set voting-power
            { holder: holder }
            {
                power: power,
                delegated-to: none,
                last-update: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (delegate-voting-power (delegate-to principal))
    (let (
        (current-power (map-get? voting-power { holder: tx-sender }))
    )
        (asserts! (is-some current-power) ERR-INSUFFICIENT-VOTING-POWER)
        (map-set voting-power
            { holder: tx-sender }
            (merge (unwrap-panic current-power) {
                delegated-to: (some delegate-to),
                last-update: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let (
        (current-power (map-get? voting-power { holder: tx-sender }))
    )
        (asserts! (is-some current-power) ERR-INSUFFICIENT-VOTING-POWER)
        (map-set voting-power
            { holder: tx-sender }
            (merge (unwrap-panic current-power) {
                delegated-to: none,
                last-update: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type (string-ascii 20)) (target-principal (optional principal)) (target-amount (optional uint)) (target-blocks (optional uint)))
    (let (
        (proposal-id (var-get total-proposals))
        (voter-power (unwrap! (get-effective-voting-power tx-sender) ERR-INSUFFICIENT-VOTING-POWER))
        (start-height stacks-block-height)
        (end-height (+ start-height (var-get voting-period-blocks)))
    )
        (asserts! (>= voter-power (var-get min-voting-power)) ERR-INSUFFICIENT-VOTING-POWER)
        (map-set proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                title: title,
                description: description,
                proposal-type: proposal-type,
                target-principal: target-principal,
                target-amount: target-amount,
                target-blocks: target-blocks,
                start-height: start-height,
                end-height: end-height,
                votes-for: u0,
                votes-against: u0,
                total-votes: u0,
                executed: false,
                passed: false
            }
        )
        (var-set total-proposals (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-power (unwrap! (get-effective-voting-power tx-sender) ERR-INSUFFICIENT-VOTING-POWER))
        (current-height stacks-block-height)
        (existing-vote (get-vote proposal-id tx-sender))
    )
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (asserts! (> voter-power u0) ERR-INSUFFICIENT-VOTING-POWER)
        (asserts! (and (>= current-height (get start-height proposal)) (<= current-height (get end-height proposal))) ERR-PROPOSAL-ENDED)
        
        (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                vote-power: voter-power,
                vote-for: vote-for,
                vote-height: current-height
            }
        )
        
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal {
                votes-for: (if vote-for (+ (get votes-for proposal) voter-power) (get votes-for proposal)),
                votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) voter-power)),
                total-votes: (+ (get total-votes proposal) voter-power)
            })
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (current-height stacks-block-height)
        (total-votes (get total-votes proposal))
        (votes-for (get votes-for proposal))
        (quorum-required (/ (* (var-get treasury-balance) (var-get quorum-percentage)) u100))
        (proposal-passed (and (>= total-votes quorum-required) (> votes-for (get votes-against proposal))))
    )
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
        (asserts! (> current-height (get end-height proposal)) ERR-VOTING-PERIOD-ACTIVE)
        (asserts! proposal-passed ERR-PROPOSAL-NOT-PASSED)
        
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal {
                executed: true,
                passed: true
            })
        )
        
        (if (is-eq (get proposal-type proposal) "create-stream")
            (let (
                (recipient (unwrap! (get target-principal proposal) ERR-INVALID-AMOUNT))
                (amount (unwrap! (get target-amount proposal) ERR-INVALID-AMOUNT))
                (blocks (unwrap! (get target-blocks proposal) ERR-INVALID-AMOUNT))
            )
                (try! (create-salary-stream recipient amount blocks))
                (ok true)
            )
            (if (is-eq (get proposal-type proposal) "change-admin")
                (let (
                    (new-admin (unwrap! (get target-principal proposal) ERR-INVALID-AMOUNT))
                )
                    (try! (set-dao-admin new-admin))
                    (ok true)
                )
                (if (is-eq (get proposal-type proposal) "fund-treasury")
                    (let (
                        (funding-amount (unwrap! (get target-amount proposal) ERR-INVALID-AMOUNT))
                    )
                        (try! (fund-treasury funding-amount))
                        (ok true)
                    )
                    (ok true)
                )
            )
        )
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
                is-active: true,
                performance-multiplier: u100,
                last-evaluation-height: start-height,
                multiplier-applied: false
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

;; Performance-based salary multiplier functions
(define-public (create-deliverable (stream-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (due-blocks uint))
    (let (
        (deliverable-id (var-get total-deliverables))
        (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
        (due-height (+ stacks-block-height due-blocks))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (> due-blocks u0) ERR-INVALID-AMOUNT)
        
        (map-set deliverables
            { deliverable-id: deliverable-id }
            {
                assignee: (get recipient stream),
                stream-id: stream-id,
                title: title,
                description: description,
                due-height: due-height,
                completed: false,
                completion-height: none,
                performance-score: none
            }
        )
        
        (var-set total-deliverables (+ deliverable-id u1))
        (ok deliverable-id)
    )
)

(define-public (complete-deliverable (deliverable-id uint))
    (let (
        (deliverable (unwrap! (get-deliverable deliverable-id) ERR-DELIVERABLE-NOT-FOUND))
        (current-height stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get assignee deliverable)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed deliverable)) ERR-DELIVERABLE-ALREADY-COMPLETED)
        
        (map-set deliverables
            { deliverable-id: deliverable-id }
            (merge deliverable {
                completed: true,
                completion-height: (some current-height)
            })
        )
        (ok true)
    )
)

(define-public (score-deliverable (deliverable-id uint) (score uint))
    (let (
        (deliverable (unwrap! (get-deliverable deliverable-id) ERR-DELIVERABLE-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (get completed deliverable) ERR-DELIVERABLE-NOT-FOUND)
        (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-SCORE)
        
        (map-set deliverables
            { deliverable-id: deliverable-id }
            (merge deliverable {
                performance-score: (some score)
            })
        )
        (ok true)
    )
)

(define-public (conduct-performance-review (stream-id uint) (review-period uint))
    (let (
        (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
        (current-height stacks-block-height)
        (evaluation-period (var-get performance-evaluation-period))
        (last-evaluation (get last-evaluation-height stream))
        (deliverables-data (calculate-performance-metrics stream-id review-period))
        (deliverable-total (get total deliverables-data))
        (deliverable-completed (get completed deliverables-data))
        (average-score (get avg-score deliverables-data))
        (completion-rate (if (> deliverable-total u0) (/ (* deliverable-completed u100) deliverable-total) u0))
        (performance-multiplier (calculate-multiplier completion-rate average-score))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (- current-height last-evaluation) evaluation-period) ERR-PERFORMANCE-PERIOD-NOT-ENDED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        
        (map-set performance-reviews
            { stream-id: stream-id, review-period: review-period }
            {
                reviewer: tx-sender,
                total-deliverables: deliverable-total,
                completed-deliverables: deliverable-completed,
                average-score: average-score,
                performance-multiplier: performance-multiplier,
                review-height: current-height,
                finalized: true
            }
        )
        
        (map-set salary-streams
            { stream-id: stream-id }
            (merge stream {
                performance-multiplier: performance-multiplier,
                last-evaluation-height: current-height,
                multiplier-applied: false
            })
        )
        
        (ok performance-multiplier)
    )
)

(define-public (apply-performance-multiplier (stream-id uint))
    (let (
        (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
        (current-multiplier (get performance-multiplier stream))
        (current-amount-per-block (get amount-per-block stream))
        (new-amount-per-block (/ (* current-amount-per-block current-multiplier) u100))
        (multiplier-applied (get multiplier-applied stream))
    )
        (asserts! (is-eq tx-sender (var-get dao-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (not multiplier-applied) ERR-MULTIPLIER-ALREADY-APPLIED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        
        (map-set salary-streams
            { stream-id: stream-id }
            (merge stream {
                amount-per-block: new-amount-per-block,
                multiplier-applied: true
            })
        )
        (ok new-amount-per-block)
    )
)

(define-private (calculate-performance-metrics (stream-id uint) (review-period uint))
    (let (
        (deliverable-count u0)
        (completed-count u0)
        (total-score u0)
        (scored-count u0)
    )
        ;; This is a simplified implementation - in practice, you'd iterate through deliverables
        ;; For now, returning sample data structure
        {
            total: u5,
            completed: u4,
            avg-score: u85
        }
    )
)

(define-private (calculate-multiplier (completion-rate uint) (average-score uint))
    (let (
        (base-multiplier u100)
        (completion-bonus (if (>= completion-rate u80) u10 u0))
        (score-bonus (if (>= average-score u90) u15 (if (>= average-score u75) u5 u0)))
        (penalty (if (< completion-rate u50) u20 u0))
    )
        (let (
            (bonus-multiplier (+ base-multiplier completion-bonus score-bonus))
            (final-multiplier (if (>= bonus-multiplier penalty) (- bonus-multiplier penalty) u50))
        )
            (if (> final-multiplier u150) u150 final-multiplier)
        )
    )
)



