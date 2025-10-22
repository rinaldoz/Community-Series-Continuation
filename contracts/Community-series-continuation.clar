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
(define-constant err-proposal-not-found (err u116))
(define-constant err-proposal-ended (err u117))
(define-constant err-already-voted (err u118))
(define-constant err-proposal-not-ended (err u119))
(define-constant err-insufficient-backing (err u120))

(define-data-var next-campaign-id uint u1)
(define-data-var total-campaigns uint u0)
(define-data-var next-milestone-id uint u1)
(define-data-var next-proposal-id uint u1)

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

(define-map proposals
  {campaign-id: uint, proposal-id: uint}
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 50),
    creator: principal,
    created-at: uint,
    voting-ends: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-power: uint,
    is-executed: bool,
    is-passed: bool
  })

(define-map proposal-votes
  {campaign-id: uint, proposal-id: uint, voter: principal}
  {
    vote: bool,
    voting-power: uint,
    timestamp: uint
  })

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

(define-public (create-proposal
  (campaign-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 50))
  (voting-duration uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (backing (map-get? campaign-backers {campaign-id: campaign-id, backer: tx-sender}))
        (proposal-id (var-get next-proposal-id))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (voting-ends (+ current-block voting-duration)))
    
    (asserts! (get is-active campaign) err-campaign-ended)
    (asserts! (or 
      (is-eq tx-sender (get creator campaign))
      (is-some backing)) 
      err-insufficient-backing)
    (asserts! (> voting-duration u0) err-invalid-amount)
    
    (map-set proposals
      {campaign-id: campaign-id, proposal-id: proposal-id}
      {
        title: title,
        description: description,
        proposal-type: proposal-type,
        creator: tx-sender,
        created-at: current-block,
        voting-ends: voting-ends,
        votes-for: u0,
        votes-against: u0,
        total-voting-power: u0,
        is-executed: false,
        is-passed: false
      })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal
  (campaign-id uint)
  (proposal-id uint)
  (vote-for bool))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (proposal (unwrap! (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id}) err-proposal-not-found))
        (backing (unwrap! (map-get? campaign-backers {campaign-id: campaign-id, backer: tx-sender}) err-insufficient-backing))
        (existing-vote (map-get? proposal-votes {campaign-id: campaign-id, proposal-id: proposal-id, voter: tx-sender}))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    
    (asserts! (< current-block (get voting-ends proposal)) err-proposal-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (> (get amount backing) u0) err-insufficient-backing)
    
    (let ((voting-power (calculate-voting-power (get amount backing) (get current-funding campaign)))
          (new-votes-for (if vote-for (+ (get votes-for proposal) voting-power) (get votes-for proposal)))
          (new-votes-against (if vote-for (get votes-against proposal) (+ (get votes-against proposal) voting-power)))
          (new-total-power (+ (get total-voting-power proposal) voting-power)))
      
      (map-set proposals
        {campaign-id: campaign-id, proposal-id: proposal-id}
        (merge proposal
          {
            votes-for: new-votes-for,
            votes-against: new-votes-against,
            total-voting-power: new-total-power
          }))
      
      (map-set proposal-votes
        {campaign-id: campaign-id, proposal-id: proposal-id, voter: tx-sender}
        {
          vote: vote-for,
          voting-power: voting-power,
          timestamp: current-block
        })
      
      (ok voting-power))))

(define-public (execute-proposal
  (campaign-id uint)
  (proposal-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) err-not-found))
        (proposal (unwrap! (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id}) err-proposal-not-found))
        (current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    
    (asserts! (>= current-block (get voting-ends proposal)) err-proposal-not-ended)
    (asserts! (not (get is-executed proposal)) err-already-approved)
    
    (let ((quorum-met (>= (get total-voting-power proposal) u5000))
          (vote-passed (> (get votes-for proposal) (get votes-against proposal)))
          (is-passed (and quorum-met vote-passed)))
      
      (map-set proposals
        {campaign-id: campaign-id, proposal-id: proposal-id}
        (merge proposal
          {
            is-executed: true,
            is-passed: is-passed
          }))
      
      (ok {
        passed: is-passed,
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        quorum-met: quorum-met
      }))))

(define-private (calculate-voting-power (backing-amount uint) (total-funding uint))
  (if (> total-funding u0)
    (/ (* backing-amount u10000) total-funding)
    u0))

(define-read-only (get-proposal (campaign-id uint) (proposal-id uint))
  (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id}))

(define-read-only (get-proposal-vote (campaign-id uint) (proposal-id uint) (voter principal))
  (map-get? proposal-votes {campaign-id: campaign-id, proposal-id: proposal-id, voter: voter}))

(define-read-only (get-voting-power (campaign-id uint) (backer principal))
  (match (map-get? campaigns campaign-id)
    campaign
    (match (map-get? campaign-backers {campaign-id: campaign-id, backer: backer})
      backing
      (some (calculate-voting-power (get amount backing) (get current-funding campaign)))
      none)
    none))

(define-read-only (get-proposal-results (campaign-id uint) (proposal-id uint))
  (match (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id})
    proposal
    (let ((total-votes (+ (get votes-for proposal) (get votes-against proposal))))
      (some {
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        total-voting-power: (get total-voting-power proposal),
        for-percentage: (if (> total-votes u0) (/ (* (get votes-for proposal) u10000) total-votes) u0),
        against-percentage: (if (> total-votes u0) (/ (* (get votes-against proposal) u10000) total-votes) u0),
        is-passed: (get is-passed proposal),
        is-executed: (get is-executed proposal),
        quorum-met: (>= (get total-voting-power proposal) u5000)
      }))
    none))

(define-read-only (can-vote (campaign-id uint) (proposal-id uint) (voter principal))
  (match (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id})
    proposal
    (let ((current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
          (has-backing (is-some (map-get? campaign-backers {campaign-id: campaign-id, backer: voter})))
          (already-voted (is-some (map-get? proposal-votes {campaign-id: campaign-id, proposal-id: proposal-id, voter: voter})))
          (voting-open (< current-block (get voting-ends proposal))))
      (some {
        can-vote: (and has-backing (not already-voted) voting-open),
        has-backing: has-backing,
        already-voted: already-voted,
        voting-open: voting-open
      }))
    none))

(define-read-only (get-proposal-status (campaign-id uint) (proposal-id uint))
  (match (map-get? proposals {campaign-id: campaign-id, proposal-id: proposal-id})
    proposal
    (let ((current-block (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
          (voting-ended (>= current-block (get voting-ends proposal)))
          (time-left (if voting-ended u0 (- (get voting-ends proposal) current-block))))
      (some {
        is-active: (not voting-ended),
        is-executed: (get is-executed proposal),
        is-passed: (get is-passed proposal),
        time-remaining: time-left,
        voting-ends: (get voting-ends proposal)
      }))
    none))

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id))
