# 🏢 Fractional Real Estate Ownership Platform

This smart contract enables fractional ownership of real estate properties through tokenized shares on the Stacks blockchain.

## 🌟 Features

- 🏗️ **Property Registration**: Register new properties with customizable shares and pricing
- 💰 **Buy & Sell Shares**: Users can purchase and sell shares in properties
- 💸 **Dividend Distribution**: Property income can be distributed to shareholders
- 📊 **Dividend Claims**: Shareholders can claim their proportional dividends
- 🔄 **Property Management**: Toggle property listing status (active/inactive)

## 📋 Contract Functions

### For Property Managers

- `register-property`: Add a new property to the platform
- `distribute-dividends`: Distribute rental income or profits to shareholders
- `toggle-property-status`: Enable or disable trading of a property's shares

### For Investors

- `buy-shares`: Purchase shares in a property
- `sell-shares`: Sell owned shares back to the platform
- `claim-dividends`: Withdraw earned dividends from property ownership

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- [Stacks Wallet](https://www.hiro.so/wallet) for interacting with the contract

### Deployment

1. Clone this repository
2. Navigate to the project directory
3. Deploy using Clarinet:

```bash
clarinet console
```

4. In the Clarinet console, deploy the contract:

```
(contract-call? .fractionalEstate register-property "Downtown Apartment" "123 Main St, New York" u1000 u50000)
```

## 💡 Usage Examples

### Register a new property (contract owner only)

```
(contract-call? .fractionalEstate register-property "Beach House" "456 Ocean Dr, Miami" u500 u100000)
```

### Buy shares in a property

```
(contract-call? .fractionalEstate buy-shares u1 u5)
```

### Distribute dividends to shareholders (contract owner only)

```
(contract-call? .fractionalEstate distribute-dividends u1 u1000000)
```

### Claim your dividends

```
(contract-call? .fractionalEstate claim-dividends u1)
```

## 📝 License

This project is licensed under the MIT License.


