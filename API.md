# Order Book REST API

A REST API wrapper around the clobbered order book library.

## Running the Server

```bash
cargo run --bin server
```

The server will start on `http://127.0.0.1:3000`

## API Endpoints

### GET /health
Health check endpoint.

**Response:**
```
"Order Book API is running"
```

### GET /orderbook
Get the current state of the order book.

**Response:**
```json
{
  "best_bid": 5000,
  "best_ask": 5500
}
```

### POST /orders
Create a new order.

**Request Body:**
```json
{
  "order_type": "limit",    // "limit", "market", "post_only", or "fill_or_kill"
  "side": "Bid",           // "Bid" or "Ask"
  "price": 5000,           // Optional for market orders, required for others
  "quantity": 100
}
```

**Response:**
```json
{
  "order_id": "550e8400-e29b-41d4-a716-446655440000",
  "log": {
    "order_id": "550e8400-e29b-41d4-a716-446655440000",
    "events": [
      {
        "Added": {
          "id": "550e8400-e29b-41d4-a716-446655440000",
          "type_": "Limit",
          "time_in_force": "GoodTillCanceled",
          "side": "Bid",
          "quantity": 100,
          "price": 5000,
          "stop_price": 0,
          "slippage": null,
          "post_only": false
        }
      }
    ]
  }
}
```

### DELETE /orders/:side/:order_id
Cancel an existing order.

**Parameters:**
- `side`: "bid" or "ask"
- `order_id`: UUID of the order

**Response:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "type_": "Limit",
  "time_in_force": "GoodTillCanceled",
  "side": "Bid",
  "quantity": 100,
  "price": 5000,
  "stop_price": 0,
  "slippage": null,
  "post_only": false
}
```

### GET /orders/:order_id
Check if an order exists (placeholder implementation).

**Response:**
```json
false
```

## Order Types

- **limit**: Limit order with specified price
- **market**: Market order executed at best available price
- **post_only**: Order that will only be added to the book if it doesn't cross the market
- **fill_or_kill**: Order that must be filled immediately and completely or be rejected

## Example Usage

Run the demo script:
```bash
./examples/api_demo.sh
```

Or test manually with curl:

```bash
# Create a bid order
curl -X POST http://127.0.0.1:3000/orders \
  -H "Content-Type: application/json" \
  -d '{
    "order_type": "limit",
    "side": "Bid", 
    "price": 5000,
    "quantity": 100
  }'

# Get order book state
curl http://127.0.0.1:3000/orderbook

# Create a market order
curl -X POST http://127.0.0.1:3000/orders \
  -H "Content-Type: application/json" \
  -d '{
    "order_type": "market",
    "side": "Ask",
    "quantity": 50
  }'
```

## Price Representation

Prices are represented as integers (u128) to avoid floating-point precision issues. For example:
- $50.00 → 5000 (using 2 decimal places)
- $55.50 → 5550 (using 2 decimal places)

Choose your own decimal precision based on your use case.