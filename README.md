# 💳 Subscription Manager Smart Contract

A **Clarity smart contract** for managing recurring subscription payments on the Stacks blockchain. Perfect for SaaS applications, membership sites, and any service requiring periodic payments! 🚀

## ✨ Features

- 🔄 **Recurring Subscriptions**: Users can subscribe and renew their subscriptions
- ⏰ **Time-based Tracking**: Automatic expiration based on block height
- 🛡️ **Grace Period**: Configurable grace period for expired subscriptions
- 📊 **Payment History**: Complete audit trail of all payments
- 🔔 **Payment Reminders**: Admin can set reminders for users
- 👑 **Admin Controls**: Owner can manage fees, durations, and force expiration
- 💰 **Fund Management**: Secure withdrawal system for contract owner

## 🏗️ Contract Structure

### Data Variables
- `subscription-fee`: Cost per subscription period (default: 1 STX)
- `subscription-duration`: Length of subscription in blocks (default: ~1 month)
- `grace-period`: Extra time after expiration (default: 144 blocks)
- `total-subscribers`: Current active subscriber count
- `contract-balance`: Total STX held by contract

### Maps
- `subscriptions`: Core subscription data per user
- `subscription-history`: Payment history with renewal tracking
- `payment-reminders`: Admin reminder system

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet with STX tokens

### Deployment

```bash
clarinet console
```

```bash
clarinet deploy
```

## 📖 Usage Guide

### 👤 User Functions

#### Subscribe to Service
```clarity
(contract-call? .subscription-manager subscribe)
```

#### Renew Subscription
```clarity
(contract-call? .subscription-manager renew-subscription)
```

#### Cancel Subscription
```clarity
(contract-call? .subscription-manager cancel-subscription)
```

#### Reactivate Cancelled Subscription
```clarity
(contract-call? .subscription-manager reactivate-subscription)
```

### 🔍 Read-Only Functions

#### Check Subscription Status
```clarity
(contract-call? .subscription-manager get-subscription-status 'SP1234...)
```

#### Check if Active
```clarity
(contract-call? .subscription-manager is-subscription-active 'SP1234...)
```

#### Get Contract Info
```clarity
(contract-call? .subscription-manager get-contract-info)
```

#### Time Until Expiry
```clarity
(contract-call? .subscription-manager time-until-expiry 'SP1234...)
```

### 👑 Admin Functions

#### Set Subscription Fee
```clarity
(contract-call? .subscription-manager set-subscription-fee u2000000)
```

#### Set Duration
````clarity
(contract
