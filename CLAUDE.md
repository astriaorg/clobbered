# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

```bash
# Build the project
cargo build

# Run tests
cargo test

# Run a specific test
cargo test test_name

# Run tests with output
cargo test -- --nocapture

# Check code without building
cargo check

# Format code
cargo fmt

# Run clippy for linting
cargo clippy
```

## Architecture Overview

This is a synchronous order book implementation in Rust called "clobbered" - deliberately free from async operations.

### Core Components

- **Book** (`src/book.rs`): The main orderbook implementation with bid/ask sides
  - Uses `Half<AskPrice>` and `Half<BidPrice>` for the two sides of the book
  - Maintains `id_to_side_and_price` HashMap for O(1) order lookups
  - Supports market, limit, post-only, stop-loss, and stop-limit orders
  - Implements price-time priority with BTreeMap for price levels

- **Order** (`src/order.rs`): Order types and primitives
  - Defines `Price`, `Quantity`, `Side` (Ask/Bid), and order `Type` enums
  - Orders have IDs, prices, quantities, sides, and types
  - Stop orders have both price and stop_price fields

- **Level** (`src/level.rs`): Price level management within the book
  - Each price level maintains a time-ordered list of orders
  - Handles order insertion, matching, and removal at a price level

- **Engine** (`src/engine.rs`): Match engine wrapper
  - Currently supports only one orderbook (no multi-symbol support yet)
  - Provides higher-level interface for order execution

- **Transaction** (`src/transaction.rs`): Event logging and transaction records
  - Tracks all events: Add, Cancel, Match, Fill, Activate
  - Used for audit trail and order execution history

### Key Design Patterns

- **Price Ordering**: `AskPrice` and `BidPrice` wrappers provide correct ordering (asks ascending, bids descending)
- **Stop Order Handling**: Stop orders are stored separately and activated when market price crosses stop price
- **Event-Driven**: All order book changes generate events in transaction logs
- **Type Safety**: Heavy use of newtype patterns for Price, Quantity, etc.

### Current Limitations

- Single orderbook only (no multi-symbol support)
- Missing order types: trailing-stop, market-to-limit, pegged, iceberg
- Missing time-in-force: good-till-date

### Testing

Tests are located in `src/book.rs` and cover various order matching scenarios. The test suite uses UUID v4 for order IDs and includes scenarios for market orders, limit orders, stop orders, and partial fills.