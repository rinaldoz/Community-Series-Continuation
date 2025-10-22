# 📺 Community Series Continuation

> 🚀 **Fan-funded TV show revival platform** - Where canceled shows get a second chance through community backing!

## 🎬 What It Does

Community Series Continuation is a decentralized platform that enables fans to collectively fund the continuation of their favorite canceled TV shows. Backers contribute STX tokens to campaign goals and earn revenue shares from streaming deals when shows get picked up.

## ✨ Key Features

- 🎯 **Campaign Creation** - Show creators can start funding campaigns
- 💰 **Community Funding** - Fans contribute STX to support their favorite shows  
- ⏰ **Time-Based Campaigns** - Each campaign has a deadline for funding goals
- 📊 **Revenue Sharing** - Backers earn proportional shares of streaming revenue
- 🔄 **Refund System** - Get your money back if campaigns don't reach their goals
- 📈 **Progress Tracking** - Real-time funding progress and statistics

## 🛠️ Smart Contract Functions

### Public Functions

#### `create-campaign`
Create a new funding campaign for a canceled show
```clarity
(create-campaign title description funding-target duration)
```

#### `fund-campaign`
Contribute STX tokens to support a campaign
```clarity
(fund-campaign campaign-id amount)
```

#### `add-streaming-revenue`
Add revenue from streaming deals (creator/owner only)
```clarity
(add-streaming-revenue campaign-id revenue)
```

#### `claim-revenue-share`
Claim your proportional share of streaming revenue
```clarity
(claim-revenue-share campaign-id)
```

#### `withdraw-funds`
Withdraw campaign funds after successful completion (creator only)
```clarity
(withdraw-funds campaign-id)
```

#### `refund-campaign`
Get refund if campaign failed to reach its goal after deadline
```clarity
(refund-campaign campaign-id)
```

### Read-Only Functions

#### `get-campaign`
Get complete campaign information
```clarity
(get-campaign campaign-id)
```

#### `get-funding-progress`
Check current funding progress and percentage
```clarity
(get-funding-progress campaign-id)
```

#### `calculate-revenue-share`
Calculate potential revenue share for a backer
```clarity
(calculate-revenue-share campaign-id backer)
```

#### `time-remaining`
Get remaining time until campaign deadline
```clarity
(time-remaining campaign-id)
```

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet with STX tokens

### Local Development

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-repo/community-series-continuation
   cd community-series-continuation
   ```

2. **Install dependencies**
   ```bash
   npm install
   ```

3. **Run checks**
   ```bash
   clarinet check
   ```

4. **Run tests**
   ```bash
   npm test
   ```

## 📋 Usage Examples

### Creating a Campaign
```clarity
;; Create a campaign for "Firefly" revival
(contract-call? .community-series-continuation create-campaign 
  "Firefly Revival" 
  "Bring back the beloved space western series" 
  u1000000 ;; 1M STX funding target
  u1440)   ;; 1440 blocks (~10 days)
```

### Funding a Campaign
```clarity
;; Fund campaign #1 with 1000 STX
(contract-call? .community-series-continuation fund-campaign u1 u1000)
```

### Claiming Revenue Share
```clarity
;; Claim your share of streaming revenue
(contract-call? .community-series-continuation claim-revenue-share u1)
```

## 📊 Campaign Lifecycle

1. 🎬 **Creation** - Show creator sets up campaign with funding goal and deadline
2. 💰 **Funding Period** - Community backs the campaign with STX tokens
3. ✅ **Success** - Campaign reaches funding goal before deadline
4. 🤝 **Production** - Show gets produced and streaming deals are negotiated  
5. 💵 **Revenue Distribution** - Backers claim their proportional shares

## 🔒 Security Features

- ⚡ **Owner Controls** - Only contract owner/creator can add streaming revenue
- 🛡️ **Validation** - All inputs are validated for security
- 💸 **Safe Transfers** - STX transfers use try/unwrap pattern
- ⏱️ **Time Locks** - Deadline enforcement prevents late contributions
- 🔄 **Refund Protection** - Automatic refunds for failed campaigns

## 💡 Revenue Calculation

Revenue shares are calculated proportionally based on contribution amounts:

```
Share % = (Your Contribution / Total Campaign Funding) × 100
Your Revenue = (Total Streaming Revenue × Share %) / 100
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙋 FAQ

**Q: What happens if a campaign doesn't reach its funding goal?**  
A: Backers can claim full refunds after the deadline passes using `refund-campaign`.

**Q: How are revenue shares calculated?**  
A: Shares are proportional to your contribution amount versus the total campaign funding.

**Q: Can I fund multiple campaigns?**  
A: Yes! You can back as many shows as you want, and your statistics are tracked globally.

**Q: What if a show never gets streaming revenue?**  
A: Campaign creators can still withdraw funds to produce content, but revenue sharing only applies when actual streaming deals generate income.

---

*Made with ❤️ by the Community Series Continuation team*
