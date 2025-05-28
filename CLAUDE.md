# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

- Build: `cargo build`
- Run tests: `cargo test`
- Check: `cargo check`
- Format: `cargo fmt`
- Lint: `cargo clippy`

## Architecture Overview

This is a synchronous order book implementation called "clobbered" - deliberately free from async operations. The core architecture consists of:

### Core Components

- **Book** (`src/book.rs`): The main orderbook implementation using a dual-sided structure with Ask/Bid price-ordered levels. Uses BTreeMap for price levels and VecDeque for order queues within each level.

- **MatchEngine** (`src/engine.rs`): Simple wrapper around a single orderbook. Currently supports only one symbol but designed to be extended for multiple orderbooks.

- **Order** (`src/order.rs`): Comprehensive order model supporting market/limit orders with various time-in-force options (GTC, IOC, FOK). Includes post-only orders and slippage control for market orders.

- **Transaction** (`src/transaction.rs`): Event logging system that tracks order additions, matches, and fills during execution.

### Key Design Decisions

- **Synchronous by design**: No async/await patterns - all operations are blocking
- **Price-time priority**: Orders at same price level are filled FIFO
- **Dual halfbook structure**: Separate Ask and Bid sides with different price ordering (ascending for asks, descending for bids)
- **Saturating arithmetic**: Price and quantity operations saturate rather than panic on overflow
- **Memory efficiency considerations**: Uses VecDeque for order queues, though comments indicate potential for optimization

### Current Limitations

- Single orderbook only (no multi-symbol support yet)
- Fill-or-kill and all-or-none orders are functionally equivalent 
- Stop orders and advanced order types are defined but not fully implemented
- No persistent storage or recovery mechanisms

The codebase is well-structured with clear separation of concerns and extensive documentation of design trade-offs and future optimization opportunities.