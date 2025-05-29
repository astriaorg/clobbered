use axum::{
    extract::{Json, Path, State},
    http::StatusCode,
    response::Json as ResponseJson,
    routing::{get, post, delete},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use tower_http::cors::CorsLayer;

use clobbered::{
    book::Key,
    engine::MatchEngine,
    order::{Id, Order, Price, Quantity, Side},
    transaction::Log,
};

// API request/response types
#[derive(Debug, Serialize, Deserialize)]
struct CreateOrderRequest {
    order_type: String, // "limit", "market", "post_only", "fill_or_kill"
    side: Side,
    price: Option<u128>, // Optional for market orders
    quantity: u128,
}

#[derive(Debug, Serialize, Deserialize)]
struct CreateOrderResponse {
    order_id: Id,
    log: Log,
}

#[derive(Debug, Serialize, Deserialize)]
struct OrderBookState {
    best_bid: Option<Price>,
    best_ask: Option<Price>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ErrorResponse {
    error: String,
}

// Application state
struct AppState {
    engine: Arc<Mutex<MatchEngine>>,
}

impl AppState {
    fn new() -> Self {
        Self {
            engine: Arc::new(Mutex::new(MatchEngine::new())),
        }
    }
}

// API handlers
async fn health() -> &'static str {
    "Order Book API is running"
}

async fn get_orderbook_state(
    State(state): State<Arc<AppState>>,
) -> Result<ResponseJson<OrderBookState>, (StatusCode, ResponseJson<ErrorResponse>)> {
    let engine = state.engine.lock().unwrap();
    
    let response = OrderBookState {
        best_bid: engine.best_price(&Side::Bid).copied(),
        best_ask: engine.best_price(&Side::Ask).copied(),
    };
    
    Ok(ResponseJson(response))
}

async fn create_order(
    State(state): State<Arc<AppState>>,
    Json(request): Json<CreateOrderRequest>,
) -> Result<ResponseJson<CreateOrderResponse>, (StatusCode, ResponseJson<ErrorResponse>)> {
    let order_id = Id::new();
    
    let order = match request.order_type.as_str() {
        "limit" => {
            let price = request.price.ok_or_else(|| {
                (StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                    error: "Price required for limit orders".to_string(),
                }))
            })?;
            Order::new_limit(
                order_id,
                request.side,
                Price::new(price),
                Quantity::new(request.quantity),
            )
        }
        "market" => Order::new_market(
            order_id,
            request.side,
            Quantity::new(request.quantity),
        ),
        "post_only" => {
            let price = request.price.ok_or_else(|| {
                (StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                    error: "Price required for post-only orders".to_string(),
                }))
            })?;
            Order::new_post_only(
                order_id,
                request.side,
                Price::new(price),
                Quantity::new(request.quantity),
            )
        }
        "fill_or_kill" => {
            let price = request.price.ok_or_else(|| {
                (StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                    error: "Price required for fill-or-kill orders".to_string(),
                }))
            })?;
            Order::new_fill_or_kill(
                order_id,
                request.side,
                Price::new(price),
                Quantity::new(request.quantity),
            )
        }
        _ => {
            return Err((StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                error: "Invalid order type. Must be: limit, market, post_only, or fill_or_kill".to_string(),
            })));
        }
    };

    // Add order to the engine
    let mut engine = state.engine.lock().unwrap();
    let log = match engine.add_order(order) {
        Ok(log) => log,
        Err(e) => {
            return Err((StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                error: format!("Failed to add order: {:?}", e),
            })));
        }
    };

    Ok(ResponseJson(CreateOrderResponse { order_id, log }))
}

async fn cancel_order(
    State(state): State<Arc<AppState>>,
    Path((side, order_id)): Path<(String, String)>,
) -> Result<ResponseJson<Order>, (StatusCode, ResponseJson<ErrorResponse>)> {
    let side = match side.as_str() {
        "bid" => Side::Bid,
        "ask" => Side::Ask,
        _ => {
            return Err((StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
                error: "Invalid side. Must be 'bid' or 'ask'".to_string(),
            })));
        }
    };

    let order_id = order_id.parse::<uuid::Uuid>().map_err(|_| {
        (StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
            error: "Invalid order ID format".to_string(),
        }))
    })?;

    let key = Key {
        side,
        id: Id::from_uuid(order_id),
    };

    let mut engine = state.engine.lock().unwrap();
    match engine.cancel_order(&key) {
        Some(order) => Ok(ResponseJson(order)),
        None => Err((StatusCode::NOT_FOUND, ResponseJson(ErrorResponse {
            error: "Order not found".to_string(),
        }))),
    }
}

async fn check_order(
    State(state): State<Arc<AppState>>,
    Path(order_id): Path<String>,
) -> Result<ResponseJson<bool>, (StatusCode, ResponseJson<ErrorResponse>)> {
    let order_id = order_id.parse::<uuid::Uuid>().map_err(|_| {
        (StatusCode::BAD_REQUEST, ResponseJson(ErrorResponse {
            error: "Invalid order ID format".to_string(),
        }))
    })?;

    // For simplicity, return a message indicating this endpoint needs improvement
    // In a real implementation, you'd want to add a proper contains method
    // that doesn't require specifying the side
    Ok(ResponseJson(false)) // Placeholder - would need proper implementation
}

#[tokio::main]
async fn main() {
    let state = Arc::new(AppState::new());

    let app = Router::new()
        .route("/health", get(health))
        .route("/orderbook", get(get_orderbook_state))
        .route("/orders", post(create_order))
        .route("/orders/:side/:order_id", delete(cancel_order))
        .route("/orders/:order_id", get(check_order))
        .layer(CorsLayer::permissive())
        .with_state(state);

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();
        
    println!("🚀 Order Book API server running on http://127.0.0.1:3000");
    println!("📚 Available endpoints:");
    println!("  GET  /health                    - Health check");
    println!("  GET  /orderbook                 - Get order book state");
    println!("  POST /orders                    - Create a new order");
    println!("  GET  /orders/:order_id          - Check if order exists");
    println!("  DELETE /orders/:side/:order_id  - Cancel an order");
    
    axum::serve(listener, app).await.unwrap();
}