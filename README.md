# 💧 DripDAO - Salary Streaming Protocol

A decentralized payroll system for DAOs built on Stacks blockchain that enables continuous salary streaming to contributors.

## 🎯 Features

- Stream STX tokens as salary payments
- Automatic per-block calculations
- Claim available salary anytime
- Treasury management
- Stream cancellation with pro-rata payments

## 📚 Contract Functions

### Administrative Functions

- `set-dao-admin`: Update the DAO administrator
- `fund-treasury`: Add STX to the treasury
- `create-salary-stream`: Create a new salary stream for a contributor
- `cancel-stream`: Cancel an active stream

### Contributor Functions

- `claim-salary`: Claim available streamed salary

### Read-Only Functions

- `get-dao-admin`: Get current admin
- `get-treasury-balance`: Check treasury balance
- `get-stream`: Get stream details
- `get-claimable-amount`: Calculate claimable amount for a stream

## 🚀 Usage Example

1. Fund the treasury:
```clarity
(contract-call? .dripdao fund-treasury u1000000000)
```

2. Create a salary stream:
```clarity
(contract-call? .dripdao create-salary-stream 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u5000000000 u4320)
```

3. Claim available salary:
```clarity
(contract-call? .dripdao claim-salary u0)
```

## ⚙️ Technical Details

- Salary amounts are in micro-STX (1 STX = 1,000,000 micro-STX)
- Stream durations are in blocks (approximately 1 block = 10 minutes)
- Streams can be claimed partially throughout their duration

## 🔒 Security

- Only DAO admin can create/cancel streams
- Only stream recipient can claim their salary
- Automatic treasury management

