# 🏡 Landmark - Decentralized Land Registry

A comprehensive smart contract for tracking real estate ownership and transfers on the Stacks blockchain. Landmark provides a transparent, immutable record of property ownership, transfers, and sales.

## 🌟 Features

- **Property Registration** 📝 - Register new properties with detailed information
- **Ownership Transfer** 🔄 - Transfer property ownership between parties
- **Property Sales** 💰 - List properties for sale and facilitate purchases
- **Transfer History** 📊 - Complete audit trail of all property transactions
- **Geographic Data** 🗺️ - Store property coordinates and location information
- **Multi-Property Management** 🏘️ - Track multiple properties per owner

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to interact with the contract

## 📋 Contract Functions

### Public Functions

#### `register-property`
Register a new property in the land registry.

**Parameters:**
- `address` - Property address (string, max 200 chars)
- `lat` - Latitude coordinate (integer)
- `lng` - Longitude coordinate (integer) 
- `size` - Property size in square units
- `property-type` - Type of property (string, max 50 chars)
- `initial-value` - Initial property value in microSTX

#### `transfer-property`
Transfer property ownership to another principal.

**Parameters:**
- `property-id` - Unique property identifier
- `new-owner` - Principal address of new owner

#### `list-for-sale`
List a property for sale at specified price.

**Parameters:**
- `property-id` - Property to list
- `price` - Sale price in microSTX

#### `buy-property`
Purchase a property that's listed for sale.

**Parameters:**
- `property-id` - Property to purchase

#### `update-property-value`
Update the assessed value of a property.

**Parameters:**
- `property-id` - Property to update
- `new-value` - New assessed value

### Read-Only Functions

#### `get-property`
Retrieve complete property information.

#### `get-property-owner`
Get the current owner of a property.

#### `get-property-history`
View transfer history for a specific transaction.

#### `get-owner-properties`
List all properties owned by a principal.

#### `get-total-properties`
Get total number of registered properties.

## 💡 Usage Examples

### Registering a Property

```bash
clarinet console
```

```clarity
(contract-call? .Landmark register-property 
  "123 Main Street, Anytown" 
  40750000 
  -73980000 
  u2500 
  "residential" 
  u1000000)
```

### Listing Property for Sale

```clarity
(contract-call? .Landmark list-for-sale u1 u1500000)
```

### Buying a Property

```clarity
(contract-call? .Landmark buy-property u1)
```

## 🔍 Error Codes

- `u100` - Unauthorized access
- `u101` - Property not found
- `u102` - Property already exists
- `u103` - Not property owner
- `u104` - Invalid price
- `u105` - Insufficient funds
- `u106` - Transfer failed
- `u107` - Property not for sale
- `u108` - Cannot buy own property

## 🛡️ Security Features

- Owner-only modifications
- Transfer validation
- Price validation
- Duplicate prevention
- Balance verification

## 🏗️ Architecture

The contract uses several data structures:
- **Properties Map** - Core property data
- **Property History** - Transfer audit trail
- **Owner Properties** - Property ownership index
- **Transfer Counter** - Transaction counting

## 🤝 Contributing

Contributions are welcome! Please ensure all code follows Clarity best practices and includes appropriate testing.

## 📄 License

This project is open source and available under the MIT License.
