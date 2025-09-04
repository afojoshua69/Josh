;; File: clarinet-winning-procurement.clar
;; A unique "winning" procurement RFP system for Google / Clarity / Web3
;; - Commit-reveal sealed proposals
;; - Evaluator allowlist + reputation-weighted scoring
;; - Finalization with tie-breakers: weighted avg -> vendor rep -> earliest reveal
;; - Milestone & final deliverable verification (sha256)
;; - On-chain award badge provenance
;; - Hook points commented for SIP-010 escrow integration (optional)
(clarity-version 2)

;; OPTIONAL: Uncomment and set the correct trait & token contract when wiring real escrow:
;; (use-trait ft-trait 'SP0000000000000000000000000000000000000000.sip010-trait)
;; (define-constant TOKEN_CONTRACT 'SP0000000000000000000000000000000000000000.token)

;; -------------------------
;; Errors
;; -------------------------
(define-constant ERR-RFP-NOT-FOUND (err u100))
(define-constant ERR-NOT-CLIENT (err u101))
(define-constant ERR-BAD-TIMING (err u102))
(define-constant ERR-ALREADY-COMMITTED (err u103))
(define-constant ERR-NO-COMMIT (err u104))
(define-constant ERR-HASH-MISMATCH (err u105))
(define-constant ERR-NOT-IN-EVAL (err u106))
(define-constant ERR-NOT-EVALUATOR (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-NO-CANDIDATES (err u109))
(define-constant ERR-NOT-WINNER (err u110))
(define-constant ERR-MILESTONE (err u111))
(define-constant ERR-BAD-ARG (err u112))

;; -------------------------
;; Status constants
;; -------------------------
(define-constant ST_COMMIT u1)
(define-constant ST_REVEAL u2)
(define-constant ST_EVALUATE u3)
(define-constant ST_AWARDED u4)
(define-constant ST_ACTIVE u5)
(define-constant ST_COMPLETED u6)

;; -------------------------
;; Globals & counters
;; -------------------------
(define-data-var rfp-counter uint u0)
(define-data-var badge-counter uint u0)

;; -------------------------
;; Storage maps
;; -------------------------
(define-map rfps
  ((id uint))
  (
    (client principal)
    (title (string-ascii 96))
    (brief (string-utf8 400))
    (min-deposit uint)        ;; logical value or token amount when integrated
    (commit-deadline uint)    ;; block-height
    (reveal-deadline uint)
    (eval-deadline uint)
    (status uint)
    (winner (optional principal))
    (badge-id (optional uint))
    (created-at uint)
  )
)

;; commits: (id, vendor) -> {commit, committed-at}
(define-map commits
  ((id uint) (vendor principal))
  (
    (commit (buff 32))
    (committed-at uint)
  )
)

;; proposals: (id, vendor) -> {uri, deposit, revealed-at, salt, deliverable (opt)}
(define-map proposals
  ((id uint) (vendor principal))
  (
    (uri (string-utf8 200))
    (deposit uint)
    (revealed-at uint)
    (salt (buff 32))
    (deliverable (optional (buff 32)))
  )
)

;; evaluators: (id, who) -> {approved bool, rep uint}
(define-map evaluators
  ((id uint) (who principal))
  (
    (approved bool)
    (rep uint)
  )
)

;; votes: (id, evaluator, vendor) -> {score uint}
(define-map votes
  ((id uint) (evaluator principal) (vendor principal))
  (
    (score uint)
  )
)

;; tallies: (id, vendor) -> {w-sum uint, w-total uint}
(define-map tallies
  ((id uint) (vendor principal))
  (
    (w-sum uint)
    (w-total uint)
  )
)

;; vendor reputation mapping (global)
(define-map vendor-rep
  ((vendor principal))
  (
    (rep uint)
  )
)

;; milestones: (id, vendor, midx) -> {deliverable (opt buff32), released bool}
(define-map milestones
  ((id uint) (vendor principal) (midx uint))
  (
    (deliverable (optional (buff 32)))
    (released bool)
  )
)

;; badges: badge-id -> {owner, meta}
(define-map badges
  ((badge-id uint))
  (
    (owner principal)
    (meta (string-utf8 200))
  )
)

;; -------------------------
;; Helpers
;; -------------------------
(define-private (only-client (id uint))
  (match (map-get? rfps { id: id })
    some (if (is-eq (get client (unwrap! some some)) tx-sender) (ok true) ERR-NOT-CLIENT)
    none ERR-RFP-NOT-FOUND))

(define-read-only (get-rfp (id uint))
  (default-to
    { client: tx-sender, title: "", brief: "", min-deposit: u0, commit-deadline: u0, reveal-deadline: u0, eval-deadline: u0, status: u0, winner: none, badge-id: none, created-at: u0}
    (map-get? rfps { id: id })))

(define-read-only (get-vendor-rep (who principal))
  (default-to { rep: u0 } (map-get? vendor-rep { vendor: who })))

(define-private (eval-weight (id uint) (who principal))
  ;; evaluator weight = 1 + floor(rep / 100)
  (match (map-get? evaluators { id: id, who: who })
    some (ok (+ u1 (div (get rep (unwrap! some some)) u100)))
    none (ok u1)
  )
)

;; -------------------------
;; Public API
;; -------------------------

;; Create a new RFP
(define-public (create-rfp
  (title (string-ascii 96))
  (brief (string-utf8 400))
  (min-deposit uint)
  (commit-deadline uint)
  (reveal-deadline uint)
  (eval-deadline uint)
)
  (begin
    (asserts! (> commit-deadline block-height) ERR-BAD-TIMING)
    (asserts! (> reveal-deadline commit-deadline) ERR-BAD-TIMING)
    (asserts! (> eval-deadline reveal-deadline) ERR-BAD-TIMING)
    (let ((id (+ (var-get rfp-counter) u1)))
      (map-set rfps { id: id }
        { client: tx-sender
        , title: title
        , brief: brief
        , min-deposit: min-deposit
        , commit-deadline: commit-deadline
        , reveal-deadline: reveal-deadline
        , eval-deadline: eval-deadline
        , status: ST_COMMIT
        , winner: none
        , badge-id: none
        , created-at: block-height })
      (var-set rfp-counter id)
      (ok id)
    )
  )
)

;; Client: approve an evaluator (and set reputation)
(define-public (approve-evaluator (id uint) (who principal) (rep uint))
  (begin
    (try! (only-client id))
    (map-set evaluators { id: id, who: who } { approved: true, rep: rep })
    (ok true)
  )
)

;; Client: set vendor reputation (for tie-break)
(define-public (set-vendor-rep (vendor principal) (rep uint))
  (begin
    ;; Governance or client may call
    (map-set vendor-rep { vendor: vendor } { rep: rep })
    (ok true)
  )
)

;; Vendor: commit proposal (commit = sha256( id || hash160(vendor) || utf8(uri) || deposit || salt ))
(define-public (commit-proposal (id uint) (commit (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (<= block-height (get commit-deadline rfp)) ERR-BAD-TIMING)
             (match (map-get? commits { id: id, vendor: tx-sender })
               some (err u103)
               none (map-set commits { id: id, vendor: tx-sender } { commit: commit, committed-at: block-height }))
             (ok true))
      none ERR-RFP-NOT-FOUND))
)

;; Vendor: reveal proposal (uri, deposit, salt). Must match earlier commit.
(define-public (reveal-proposal (id uint) (uri (string-utf8 200)) (deposit uint) (salt (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (asserts! (> block-height (get commit-deadline rfp)) ERR-BAD-TIMING)
             (asserts! (<= block-height (get reveal-deadline rfp)) ERR-BAD-TIMING)
             (let ((c (map-get? commits { id: id, vendor: tx-sender })))
               (match c
                 somec (let ((stored (get commit (unwrap! somec somec)))
                             (computed (sha256 (concat
                                                 (to-buff id)
                                                 (concat (to-buff (hash160 tx-sender))
                                                         (concat (utf8-to-bytes uri)
                                                                 (concat (to-buff deposit) salt)))))))
                         (asserts! (is-eq stored computed) ERR-HASH-MISMATCH)
                         ;; OPTIONAL: transfer deposit to contract here (SIP-010) to lock funds:
                         ;; (try! (ft-transfer TOKEN_CONTRACT tx-sender (as-contract) deposit))
                         (map-set proposals { id: id, vendor: tx-sender } { uri: uri, deposit: deposit, revealed-at: block-height, salt: salt, deliverable: none })
                         (ok true))
                 none (err u104))))
      none ERR-RFP-NOT-FOUND))
)

;; Client: start evaluation (after reveal deadline)
(define-public (start-evaluation (id uint))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (try! (only-client id))
             (asserts! (> block-height (get reveal-deadline rfp)) ERR-BAD-TIMING)
             (map-set rfps { id: id } (merge rfp { status: ST_EVALUATE }))
             (ok true))
      none ERR-RFP-NOT-FOUND))
)

;; Evaluator: cast score (0..100). Weight by evaluator rep.
(define-public (cast-score (id uint) (vendor principal) (score uint))
  (begin
    (asserts! (<= score u100) ERR-BAD-ARG)
    (let ((r (map-get? rfps { id: id })))
      (match r
        some (let ((rfp (unwrap! some some)))
               (asserts! (is-eq (get status rfp) ST_EVALUATE) ERR-NOT-IN-EVAL)
               (asserts! (<= block-height (get eval-deadline rfp)) ERR-BAD-TIMING)
               (let ((ev (map-get? evaluators { id: id, who: tx-sender })))
                 (match ev
                   some-ev (let ((e (unwrap! some-ev some-ev)))
                             (asserts! (get approved e) ERR-NOT-EVALUATOR)
                             ;; vendor must have revealed
                             (match (map-get? proposals { id: id, vendor: vendor })
                               somep (let ((existing-vote (map-get? votes { id: id, evaluator: tx-sender, vendor: vendor })))
                                       (match existing-vote
                                         some (err u108)
                                         none (let ((w (try! (eval-weight id tx-sender))))
                                                (map-set votes { id: id, evaluator: tx-sender, vendor: vendor } { score: score })
                                                (match (map-get? tallies { id: id, vendor: vendor })
                                                  some-t (let ((t (unwrap! some-t some-t)))
                                                           (map-set tallies { id: id, vendor: vendor } { w-sum: (+ (get w-sum t) (* score w)), w-total: (+ (get w-total t) w) }))
                                                  none (map-set tallies { id: id, vendor: vendor } { w-sum: (* score w), w-total: w }))
                                                (ok true)))) 
                               none (err u104)))
                   none (err u107))))
        none ERR-RFP-NOT-FOUND))
  )
)

;; Finalize winner: client provides candidate vendor list; contract picks highest weighted average
;; Tie-breakers: weighted avg -> vendor rep -> earliest revealed-at
(define-public (finalize (id uint) (candidates (list 200 principal)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (try! (only-client id))
             (asserts! (is-eq (get status rfp) ST_EVALUATE) ERR-NOT-IN-EVAL)
             (asserts! (> block-height (get eval-deadline rfp)) ERR-BAD-TIMING)
             (let ((result
                    (fold candidates none
                      (lambda (cand acc)
                        (let ((t (map-get? tallies { id: id, vendor: cand }))
                              (p (map-get? proposals { id: id, vendor: cand })))
                          (if (is-some t)
                              (let ((ws (get w-sum (unwrap! t t))) (wt (get w-total (unwrap! t t))) (vrep (get rep (default-to { rep: u0 } (map-get? vendor-rep { vendor: cand })))) (rev (get revealed-at (unwrap! p p))))
                                (if (is-none acc)
                                    (some (tuple cand ws wt vrep rev))
                                    (let ((accv (unwrap! acc acc)))
                                      (let ((acand (get 0 accv)) (aws (get 1 accv)) (awt (get 2 accv)) (arep (get 3 accv)) (arev (get 4 accv)))
                                        (let ((cmp (* ws awt) (* aws wt))) ;; compare ws/wt vs aws/awt
                                          (if (> cmp (* aws wt))
                                              (some (tuple cand ws wt vrep rev))
                                              (if (is-eq cmp (* aws wt))
                                                  (if (> vrep arep)
                                                      (some (tuple cand ws wt vrep rev))
                                                      (if (is-eq vrep arep)
                                                          (if (< rev arev) (some (tuple cand ws wt vrep rev)) acc)
                                                          acc))
                                                  acc)))))))
                              acc)))))))
               (match result
                 none (err u109)
                 some-res (let ((winner (get 0 (unwrap! some-res some-res))) (wsum (get 1 (unwrap! some-res some-res))) (wtotal (get 2 (unwrap! some-res some-res))))
                            (let ((bid (+ (var-get badge-counter) u1)))
                              (var-set badge-counter bid)
                              (map-set badges { badge-id: bid } { owner: winner, meta: (concat "Award:RFP#" (to-hex (to-buff id))) })
                              (map-set rfps { id: id } (merge rfp { status: ST_AWARDED, winner: (some winner), badge-id: (some bid) }))
                              ;; OPTIONAL: payout via SIP-010 to winner here:
                              ;; (try! (ft-transfer TOKEN_CONTRACT tx-sender winner payout-amount))
                              (ok (tuple (winner winner) (badge-id bid) (w-sum wsum) (w-total wtotal))))))))
      none ERR-RFP-NOT-FOUND))
)

;; Milestone: vendor posts deliverable for milestone index (midx)
(define-public (post-milestone (id uint) (midx uint) (deliverable (buff 32)))
  (match (map-get? proposals { id: id, vendor: tx-sender })
    some (map-set milestones { id: id, vendor: tx-sender, midx: midx } { deliverable: (some deliverable), released: false }) (ok true)
    none (err u104))
)

;; Client: verify milestone and release payment (client-authorized)
(define-public (verify-release (id uint) (vendor principal) (midx uint) (expected (buff 32)) (release-amount uint))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (try! (only-client id))
             (match (map-get? milestones { id: id, vendor: vendor, midx: midx })
               some (let ((mrec (unwrap! some some)))
                      (let ((del (get deliverable mrec)))
                        (match del
                          some-d (if (is-eq some-d expected)
                                     (begin
                                       (map-set milestones { id: id, vendor: vendor, midx: midx } (merge mrec { released: true }))
                                       ;; OPTIONAL: token payout via SIP-010
                                       ;; (try! (ft-transfer TOKEN_CONTRACT tx-sender vendor release-amount))
                                       (ok true))
                                     (err u111))
                          none (err u111))))
               none (err u111)))
      none ERR-RFP-NOT-FOUND))
)

;; Winner posts final deliverable hash
(define-public (post-final (id uint) (deliverable (buff 32)))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (match (get winner rfp)
               some-w (let ((w (unwrap! some-w some-w)))
                        (match (map-get? proposals { id: id, vendor: w })
                          somep (map-set proposals { id: id, vendor: w } (merge (unwrap! somep somep) { deliverable: (some deliverable) })) (ok true)
                          none (err u104)))
               none (err u110)))
      none ERR-RFP-NOT-FOUND))
)

;; Client: verify final deliverable and complete (bump vendor rep)
(define-public (verify-final (id uint) (expected (buff 32)) (rep-bump uint))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (let ((rfp (unwrap! some some)))
             (try! (only-client id))
             (match (get winner rfp)
               some-w (let ((w (unwrap! some-w some-w)))
                        (match (map-get? proposals { id: id, vendor: w })
                          somep (let ((prec (unwrap! somep somep)))
                                  (match (get deliverable prec)
                                    some-d (if (is-eq some-d expected)
                                               (begin
                                                 ;; bump rep
                                                 (let ((cur (default-to { rep: u0 } (map-get? vendor-rep { vendor: w }))))
                                                   (map-set vendor-rep { vendor: w } { rep: (+ (get rep cur) rep-bump) })
                                                   (map-set rfps { id: id } (merge rfp { status: ST_COMPLETED }))
                                                   (ok true))
                                               (err u105))
                                    none (err u111)))
                          none (err u104)))
               none (err u110)))
      none ERR-RFP-NOT-FOUND))
)

;; Read-only helpers
(define-read-only (get-winner (id uint))
  (match (map-get? rfps { id: id })
    some (ok (get winner (unwrap! some some)))
    none (ok none))
)

(define-read-only (get-badge (bid uint))
  (default-to { owner: 'SP0000000000000000000000000000000000000000, meta: "" } (map-get? badges { badge-id: bid }))
)
