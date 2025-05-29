#!/bin/bash

# Order Book API Demo Script
# Make sure the server is running: cargo run --bin server

BASE_URL="http://127.0.0.1:3000"

echo "🚀 Order Book API Demo"
echo "====================="

echo
echo "1. Health check:"
curl -s "$BASE_URL/health"
echo

echo
echo "2. Get initial order book state:"
curl -s "$BASE_URL/orderbook" | jq '.'

echo
echo "3. Create a bid limit order (buy 100 shares at $50):"
BID_RESPONSE=$(curl -s -X POST "$BASE_URL/orders" \
  -H "Content-Type: application/json" \
  -d '{
    "order_type": "limit",
    "side": "Bid",
    "price": 5000,
    "quantity": 100
  }')
echo "$BID_RESPONSE" | jq '.'
BID_ORDER_ID=$(echo "$BID_RESPONSE" | jq -r '.order_id')

echo
echo "4. Create an ask limit order (sell 50 shares at $55):"
ASK_RESPONSE=$(curl -s -X POST "$BASE_URL/orders" \
  -H "Content-Type: application/json" \
  -d '{
    "order_type": "limit",
    "side": "Ask",
    "price": 5500,
    "quantity": 50
  }')
echo "$ASK_RESPONSE" | jq '.'
ASK_ORDER_ID=$(echo "$ASK_RESPONSE" | jq -r '.order_id')

echo
echo "5. Get order book state after adding orders:"
curl -s "$BASE_URL/orderbook" | jq '.'

echo
echo "6. Create a market buy order (buy 30 shares at market price):"
MARKET_RESPONSE=$(curl -s -X POST "$BASE_URL/orders" \
  -H "Content-Type: application/json" \
  -d '{
    "order_type": "market",
    "side": "Bid",
    "quantity": 30
  }')
echo "$MARKET_RESPONSE" | jq '.'

echo
echo "7. Get order book state after market order:"
curl -s "$BASE_URL/orderbook" | jq '.'

echo
echo "8. Cancel the bid order:"
curl -s -X DELETE "$BASE_URL/orders/bid/$BID_ORDER_ID" | jq '.'

echo
echo "9. Final order book state:"
curl -s "$BASE_URL/orderbook" | jq '.'

echo
echo "Demo completed! 🎉"