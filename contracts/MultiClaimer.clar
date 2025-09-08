(define-constant CONTRACT-DRIPDAO .Dripdao)

;; Multi-claim helper for Dripdao salary streams
;; - claim-salaries: claim multiple stream IDs in one tx, returning the total claimed
;; - preview-total-claimable: public preview of total claimable across stream IDs
;;
;; Notes:
;; - Errors from individual streams are ignored (e.g., not recipient, inactive, zero claimable)
;;   to allow best-effort batch claiming.
;; - The max list size is capped for simplicity and to stay within runtime limits.

(define-private (claim-once (acc uint) (stream-id uint))
  (let ((res (contract-call? CONTRACT-DRIPDAO claim-salary stream-id)))
    (match res 
      claimed (+ acc claimed)
      err acc)))

(define-public (claim-salaries (stream-ids (list 20 uint)))
  (ok (fold claim-once stream-ids u0)))

(define-private (preview-once (acc uint) (stream-id uint))
  (let ((res (contract-call? CONTRACT-DRIPDAO get-claimable-amount stream-id)))
    (match res 
      amount (+ acc amount)
      err acc)))

(define-public (preview-total-claimable (stream-ids (list 20 uint)))
  (ok (fold preview-once stream-ids u0)))

