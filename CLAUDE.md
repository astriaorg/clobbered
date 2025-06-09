# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a synchronous orderbook implementation in Rust called "clobbered". It's designed to be fully synchronous (async-free) and supports various order types and time-in-force options.

**Current functionality:**
- Multiple orderbooks (one per trading symbol)
- Order types: market, limit, post-only, stop-loss, stop-limit
- Time-in-force: good-till-cancelled, immediate-or-cancel, fill-or-kill, all-or-none

**Planned functionality:**
- Additional order types: trailing-stop, market-to-limit, pegged, iceberg/reserved
- Additional time-in-force: good-till-date

## Common Commands

```bash
# Build the project
cargo build

# Run tests
cargo test

# Run a specific test
cargo test <test_name>

# Run tests with output
cargo test -- --nocapture

# Check code without building
cargo check

# Format code
cargo fmt

# Lint code
cargo clippy
```

## Architecture

### Core Components

- **`book.rs`**: The main orderbook implementation with `Book` struct containing bid/ask halves
- **`order.rs`**: Order types, pricing, quantities, symbols, and order builder pattern
- **`level.rs`**: Price-time levels that store orders at the same price
- **`engine.rs`**: Match engine that manages multiple orderbooks (one per symbol)
- **`transaction.rs`**: Transaction logging and event tracking

### Key Design Patterns

- **Multi-Symbol Support**: Each trading symbol has its own independent orderbook managed by `MatchEngine`
- **Price-Time Priority**: Orders are matched first by price, then by time of arrival
- **Half-Book Architecture**: Separate `Ask` and `Bid` halves with different price ordering (ascending for asks, descending for bids)
- **Stop Order Management**: Stop orders are stored separately and activated when market conditions are met
- **Event Logging**: All order lifecycle events (add, match, fill, cancel, activate) are logged via `transaction::Log`

### Order Execution Flow

1. Orderbooks must be explicitly created via `MatchEngine::create_orderbook()`
2. Orders enter via `MatchEngine::add_order()` with their symbol
3. Engine routes order to appropriate symbol's orderbook (fails if not created)
4. Post-only orders are rejected if they would cross the market
5. Executable orders are matched against the opposite side within the same symbol
6. Remaining volume is added to the book (unless immediate execution required)
7. Stop orders are activated if market price triggers them
8. Full matching loop ensures all possible matches occur

### Important Implementation Details

- **Explicit Orderbook Creation**: Orderbooks must be created before orders can be added for a symbol
- **Symbol Isolation**: Orders only match within the same symbol - no cross-symbol matching
- **Order Builder**: All orders must specify a symbol via `Order::builder().symbol("BTC")`
- **Unknown Symbol Handling**: Adding orders for non-existent symbols returns `ExecutionError::UnknownSymbol`
- Orders with zero quantity are marked as filled but may remain in levels until cleanup
- Price types (`AskPrice`/`BidPrice`) handle different sorting requirements automatically  
- Stop orders use `stop_price` for activation, then become regular limit/market orders
- Market orders can have slippage protection to limit execution price range
- Order cancellation searches across all symbols since order IDs are globally unique