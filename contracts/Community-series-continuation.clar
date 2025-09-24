(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-campaign-ended (err u104))
(define-constant err-campaign-active (err u105))
(define-constant err-already-funded (err u106))
(define-constant err-not-funded (err u107))
(define-constant err-invalid-amount (err u108))
(define-constant err-unauthorized (err u109))
(define-constant err-milestone-not-found (err u110))
(define-constant err-milestone-completed (err u111))
(define-constant err-milestone-not-completed (err u112))
(define-constant err-insufficient-milestone-funds (err u113))
(define-constant err-approval-required (err u114))
(define-constant err-already-approved (err u115))

(define-data-var next-campaign-id uint u1)
(define-data-var total-campaigns uint u0)
(define-data-var next-milestone-id uint u1)

(define-map campaigns 
  uint 
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    funding-target: uint,
    current-funding: uint,
    deadline: uint,
    is-funded: bool,
    is-active: bool,
    streaming-revenue: uint,
    total-backers: uint
  })

(define-map campaign-backers 
  {campaign-id: uint, backer: principal}
  {amount: uint, revenue-share: uint})

(define-map user-contributions 
  principal 
  {total-contributed: uint, campaigns-backed: uint})

(define-map campaign-milestones
  {campaign-id: uint, milestone-id: uint}
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-target: uint,
    current-funding: uint,
    is-completed: bool,
    is-approved: bool,
    completion-proof: (optional (string-ascii 200)),
    order-index: uint
  })

(define-map milestone-backers
  {campaign-id: uint, milestone-id: uint, backer: principal}
  {amount: uint})

(define-map milestone-approvals
  {campaign-id: uint, milestone-id: uint, approver: principal}
  {approved: bool, timestamp: uint})

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (funding-target uint) (duration uint))
  (let ((campaign-id (var-get next-campaign-id))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (deadline (+ current-block duration)))
    (asserts! (> funding-target u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    (map-set campaigns campaign-id
      {
        title: title,
        description: description,
        creator: tx-sender,
        funding-target: funding-target,
        current-funding: u0,
        deadline: deadline,
        is-funded: false,
        is-active: true,
        streaming-revenue: u0,
        total-backers: u0
      })
    (var-set next-campaign-id (+ campaign-id u1))
    (var-set total-campaigns (+ (var-get total-campaigns) u1))
    (ok campaign-id)))

(define-public (fund-campaign (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (user-stats (default-to {total-contributed: u0, campaigns-backed: u0} 
                                (map-get? user-contributions tx-sender)))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (get is-active campaign) err-campaign-ended)
    (asserts! (< current-block (get deadline campaign)) err-campaign-ended)
    (asserts! (not (get is-funded campaign)) err-already-funded)
    (asserts! (> amount u0) err-invalid-amount)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (let ((new-funding (+ (get current-funding campaign) amount))
          (is-now-funded (>= new-funding (get funding-target campaign)))
          (existing-backing (map-get? campaign-backers {campaign-id: campaign-id, backer: tx-sender}))
          (is-new-backer (is-none existing-backing)))
      
      (map-set campaigns campaign-id
        (merge campaign 
          {current-funding: new-funding,
           is-funded: is-now-funded,
           total-backers: (if is-new-backer 
                           (+ (get total-backers campaign) u1) 
                           (get total-backers campaign))}))
      
      (map-set campaign-backers 
        {campaign-id: campaign-id, backer: tx-sender}
        {amount: (+ (default-to u0 (get amount existing-backing)) amount),
         revenue-share: u0})
      
      (map-set user-contributions tx-sender
        {total-contributed: (+ (get total-contributed user-stats) amount),
         campaigns-backed: (if is-new-backer 
                           (+ (get campaigns-backed user-stats) u1)
                           (get campaigns-backed user-stats))})
      
      (ok new-funding))))

(define-public (add-streaming-revenue (campaign-id uint) (revenue uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-eq tx-sender (get creator campaign))) err-unauthorized)
    (asserts! (get is-funded campaign) err-not-funded)
    (asserts! (> revenue u0) err-invalid-amount)
    
    (try! (stx-transfer? revenue tx-sender (as-contract tx-sender)))
    
    (map-set campaigns campaign-id
      (merge campaign 
        {streaming-revenue: (+ (get streaming-revenue campaign) revenue)}))
    (ok true)))

(define-public (claim-revenue-share (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (backing (unwrap! (map-get? campaign-backers 
                                   {campaign-id: campaign-id, backer: tx-sender}) 
                         err-not-found)))
    (asserts! (get is-funded campaign) err-not-funded)
    (asserts! (> (get streaming-revenue campaign) u0) err-invalid-amount)
    (asserts! (is-eq (get revenue-share backing) u0) err-already-funded)
    
    (let ((share-percentage (/ (* (get amount backing) u10000) (get current-funding campaign)))
          (revenue-share (/ (* (get streaming-revenue campaign) share-percentage) u10000)))
      
      (map-set campaign-backers 
        {campaign-id: campaign-id, backer: tx-sender}
        (merge backing {revenue-share: revenue-share}))
      
      (try! (as-contract (stx-transfer? revenue-share tx-sender tx-sender)))
      (ok revenue-share))))

(define-public (withdraw-funds (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found)))
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (get is-funded campaign) err-not-funded)
    (asserts! (get is-active campaign) err-campaign-active)
    
    (map-set campaigns campaign-id
      (merge campaign {is-active: false}))
    
    (try! (as-contract (stx-transfer? (get current-funding campaign) tx-sender (get creator campaign))))
    (ok (get current-funding campaign))))

(define-public (refund-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (backing (unwrap! (map-get? campaign-backers 
                                   {campaign-id: campaign-id, backer: tx-sender}) 
                         err-not-found))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (not (get is-funded campaign)) err-already-funded)
    (asserts! (>= current-block (get deadline campaign)) err-campaign-active)
    (asserts! (> (get amount backing) u0) err-invalid-amount)
    
    (map-delete campaign-backers {campaign-id: campaign-id, backer: tx-sender})
    
    (try! (as-contract (stx-transfer? (get amount backing) tx-sender tx-sender)))
    (ok (get amount backing))))

(define-public (close-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (>= current-block (get deadline campaign)) err-campaign-active)
    (asserts! (get is-active campaign) err-campaign-ended)
    
    (map-set campaigns campaign-id
      (merge campaign {is-active: false}))
    (ok true)))

(define-public (create-milestone 
  (campaign-id uint) 
  (title (string-ascii 100)) 
  (description (string-ascii 300)) 
  (funding-target uint) 
  (order-index uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (milestone-id (var-get next-milestone-id)))
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-ended)
    (asserts! (> funding-target u0) err-invalid-amount)
    
    (map-set campaign-milestones 
      {campaign-id: campaign-id, milestone-id: milestone-id}
      {
        title: title,
        description: description,
        funding-target: funding-target,
        current-funding: u0,
        is-completed: false,
        is-approved: false,
        completion-proof: none,
        order-index: order-index
      })
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)))

(define-public (fund-milestone (campaign-id uint) (milestone-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (milestone (unwrap! (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id}) err-milestone-not-found))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (existing-backing (map-get? milestone-backers {campaign-id: campaign-id, milestone-id: milestone-id, backer: tx-sender})))
    (asserts! (get is-active campaign) err-campaign-ended)
    (asserts! (< current-block (get deadline campaign)) err-campaign-ended)
    (asserts! (not (get is-completed milestone)) err-milestone-completed)
    (asserts! (> amount u0) err-invalid-amount)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (let ((new-funding (+ (get current-funding milestone) amount)))
      (map-set campaign-milestones 
        {campaign-id: campaign-id, milestone-id: milestone-id}
        (merge milestone {current-funding: new-funding}))
      
      (map-set milestone-backers 
        {campaign-id: campaign-id, milestone-id: milestone-id, backer: tx-sender}
        {amount: (+ (default-to u0 (get amount existing-backing)) amount)})
      
      (ok new-funding))))

(define-public (complete-milestone (campaign-id uint) (milestone-id uint) (completion-proof (string-ascii 200)))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (milestone (unwrap! (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id}) err-milestone-not-found)))
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (>= (get current-funding milestone) (get funding-target milestone)) err-insufficient-milestone-funds)
    (asserts! (not (get is-completed milestone)) err-milestone-completed)
    
    (map-set campaign-milestones 
      {campaign-id: campaign-id, milestone-id: milestone-id}
      (merge milestone 
        {
          is-completed: true,
          completion-proof: (some completion-proof)
        }))
    
    (ok true)))

(define-public (claim-milestone-funds (campaign-id uint) (milestone-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (milestone (unwrap! (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id}) err-milestone-not-found)))
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (get is-completed milestone) err-milestone-not-completed)
    (asserts! (get is-approved milestone) err-approval-required)
    
    (try! (as-contract (stx-transfer? (get current-funding milestone) tx-sender (get creator campaign))))
    (ok (get current-funding milestone))))

(define-public (approve-milestone (campaign-id uint) (milestone-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (milestone (unwrap! (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id}) err-milestone-not-found))
        (existing-approval (map-get? milestone-approvals {campaign-id: campaign-id, milestone-id: milestone-id, approver: tx-sender})))
    (asserts! (get is-completed milestone) err-milestone-not-completed)
    (asserts! (is-none existing-approval) err-already-approved)
    (asserts! (or 
      (is-eq tx-sender contract-owner)
      (is-some (map-get? milestone-backers {campaign-id: campaign-id, milestone-id: milestone-id, backer: tx-sender}))) 
      err-unauthorized)
    
    (map-set milestone-approvals 
      {campaign-id: campaign-id, milestone-id: milestone-id, approver: tx-sender}
      {approved: true, timestamp: stacks-block-height})
    
    (let ((approval-count (get-milestone-approval-count campaign-id milestone-id))
          (min-approvals (/ (get total-backers campaign) u3))
          (required-approvals (if (> min-approvals u1) min-approvals u1)))
      (if (>= approval-count required-approvals)
        (begin
          (map-set campaign-milestones 
            {campaign-id: campaign-id, milestone-id: milestone-id}
            (merge milestone {is-approved: true}))
          (ok {approved: true, auto-approved: true}))
        (ok {approved: true, auto-approved: false})))))

(define-private (get-milestone-approval-count (campaign-id uint) (milestone-id uint))
  u1)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id))

(define-read-only (get-campaign-backing (campaign-id uint) (backer principal))
  (map-get? campaign-backers {campaign-id: campaign-id, backer: backer}))

(define-read-only (get-user-stats (user principal))
  (map-get? user-contributions user))

(define-read-only (get-total-campaigns)
  (var-get total-campaigns))

(define-read-only (get-next-campaign-id)
  (var-get next-campaign-id))

(define-read-only (calculate-revenue-share (campaign-id uint) (backer principal))
  (match (map-get? campaigns campaign-id)
    campaign
    (match (map-get? campaign-backers {campaign-id: campaign-id, backer: backer})
      backing
      (if (and (get is-funded campaign) (> (get streaming-revenue campaign) u0))
        (let ((share-percentage (/ (* (get amount backing) u10000) (get current-funding campaign))))
          (some (/ (* (get streaming-revenue campaign) share-percentage) u10000)))
        (some u0))
      none)
    none))

(define-read-only (is-campaign-funded (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign (some (get is-funded campaign))
    none))

(define-read-only (get-funding-progress (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign 
    (some {
      current: (get current-funding campaign),
      target: (get funding-target campaign),
      percentage: (/ (* (get current-funding campaign) u10000) (get funding-target campaign))
    })
    none))

(define-read-only (time-remaining (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (let ((current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
      (if (> (get deadline campaign) current-block)
        (some (- (get deadline campaign) current-block))
        (some u0)))
    none))

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id}))

(define-read-only (get-milestone-backing (campaign-id uint) (milestone-id uint) (backer principal))
  (map-get? milestone-backers {campaign-id: campaign-id, milestone-id: milestone-id, backer: backer}))

(define-read-only (get-milestone-funding-progress (campaign-id uint) (milestone-id uint))
  (match (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id})
    milestone
    (some {
      current: (get current-funding milestone),
      target: (get funding-target milestone),
      percentage: (/ (* (get current-funding milestone) u10000) (get funding-target milestone)),
      is-funded: (>= (get current-funding milestone) (get funding-target milestone))
    })
    none))

(define-read-only (is-milestone-approved (campaign-id uint) (milestone-id uint))
  (match (map-get? campaign-milestones {campaign-id: campaign-id, milestone-id: milestone-id})
    milestone (some (get is-approved milestone))
    none))

(define-read-only (get-milestone-approval (campaign-id uint) (milestone-id uint) (approver principal))
  (map-get? milestone-approvals {campaign-id: campaign-id, milestone-id: milestone-id, approver: approver}))

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id))
