use crate::{order, transaction};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExecutionError {
    AddOrder(Box<crate::book::AddOrderError>),
}

impl From<crate::book::AddOrderError> for ExecutionError {
    fn from(value: crate::book::AddOrderError) -> Self {
        Self::AddOrder(Box::new(value))
    }
}

/// A match engine tracks multiple orderbooks, one per trading symbol.
pub struct MatchEngine {
    orderbooks: HashMap<order::Symbol, crate::book::Book>,
}

impl MatchEngine {
    pub fn new() -> Self {
        Self {
            orderbooks: HashMap::new(),
        }
    }

    /// Creates a new orderbook for the given symbol if it doesn't exist.
    pub fn create_orderbook(&mut self, symbol: order::Symbol) {
        self.orderbooks.entry(symbol).or_insert_with(crate::book::Book::new);
    }

    /// Adds an order to the appropriate orderbook based on the order's symbol.
    /// Creates a new orderbook for the symbol if it doesn't exist.
    pub fn add_order(&mut self, order: order::Order) -> Result<transaction::Log, ExecutionError> {
        let symbol = order.symbol().clone();
        let orderbook = self.orderbooks.entry(symbol).or_insert_with(crate::book::Book::new);
        
        let mut log = transaction::Log::new();
        orderbook.execute(order, &mut log)?;
        Ok(log)
    }

    /// Returns a reference to the orderbook for the given symbol, if it exists.
    pub fn get_orderbook(&self, symbol: &order::Symbol) -> Option<&crate::book::Book> {
        self.orderbooks.get(symbol)
    }

    /// Returns a mutable reference to the orderbook for the given symbol, if it exists.
    pub fn get_orderbook_mut(&mut self, symbol: &order::Symbol) -> Option<&mut crate::book::Book> {
        self.orderbooks.get_mut(symbol)
    }

    /// Returns all symbols that have orderbooks.
    pub fn symbols(&self) -> impl Iterator<Item = &order::Symbol> {
        self.orderbooks.keys()
    }

    /// Cancels an order by ID. Returns true if the order was found and cancelled.
    /// Note: This searches across all orderbooks since order IDs should be globally unique.
    pub fn cancel_order(&mut self, order_id: &order::Id) -> Result<Option<transaction::Log>, ExecutionError> {
        for orderbook in self.orderbooks.values_mut() {
            let mut log = transaction::Log::new();
            if orderbook.cancel(order_id, &mut log) {
                return Ok(Some(log));
            }
        }
        Ok(None)
    }
}

impl Default for MatchEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::order::{Id, Order, Price, Quantity, Side, Symbol};
    use uuid::Uuid;

    fn create_order(symbol: &str, side: Side, price: u128, quantity: u128) -> Order {
        Order::builder()
            .id(Id::new(Uuid::new_v4()))
            .symbol(symbol)
            .side(side)
            .price(Price::new(price))
            .quantity(Quantity::new(quantity))
            .build()
            .unwrap()
    }

    #[test]
    fn engine_creates_orderbook_for_new_symbol() {
        let mut engine = MatchEngine::new();
        let btc_order = create_order("BTC", Side::Bid, 50000, 1);
        
        engine.add_order(btc_order).unwrap();
        
        assert!(engine.get_orderbook(&Symbol::from("BTC")).is_some());
        assert_eq!(engine.symbols().count(), 1);
    }

    #[test]
    fn engine_handles_multiple_symbols() {
        let mut engine = MatchEngine::new();
        
        let btc_order = create_order("BTC", Side::Bid, 50000, 1);
        let eth_order = create_order("ETH", Side::Ask, 3000, 5);
        
        engine.add_order(btc_order).unwrap();
        engine.add_order(eth_order).unwrap();
        
        assert!(engine.get_orderbook(&Symbol::from("BTC")).is_some());
        assert!(engine.get_orderbook(&Symbol::from("ETH")).is_some());
        assert_eq!(engine.symbols().count(), 2);
    }

    #[test]
    fn orders_only_match_within_same_symbol() {
        let mut engine = MatchEngine::new();
        
        // Add BTC ask order
        let btc_ask = create_order("BTC", Side::Ask, 50000, 1);
        engine.add_order(btc_ask).unwrap();
        
        // Add ETH bid order at higher price - should not match with BTC
        let eth_bid = create_order("ETH", Side::Bid, 60000, 1);
        let log = engine.add_order(eth_bid).unwrap();
        
        // ETH bid should be added to book (not matched)
        assert!(log.events.iter().any(|e| e.is_add()));
        assert!(!log.events.iter().any(|e| e.is_match()));
        
        // Both orderbooks should exist and have orders
        let btc_book = engine.get_orderbook(&Symbol::from("BTC")).unwrap();
        let eth_book = engine.get_orderbook(&Symbol::from("ETH")).unwrap();
        
        assert!(btc_book.best_price(&Side::Ask).is_some());
        assert!(eth_book.best_price(&Side::Bid).is_some());
    }

    #[test]
    fn cancel_order_searches_all_symbols() {
        let mut engine = MatchEngine::new();
        
        let order_id = Id::new(Uuid::new_v4());
        let btc_order = Order::builder()
            .id(order_id)
            .symbol("BTC")
            .side(Side::Bid)
            .price(Price::new(50000))
            .quantity(Quantity::new(1))
            .build()
            .unwrap();
        
        engine.add_order(btc_order).unwrap();
        
        // Cancel should find the order even without specifying symbol
        let cancel_log = engine.cancel_order(&order_id).unwrap();
        assert!(cancel_log.is_some());
        
        let log = cancel_log.unwrap();
        assert!(log.events.iter().any(|e| matches!(e, transaction::Event::Cancel { .. })));
    }

    #[test]
    fn create_orderbook_explicitly() {
        let mut engine = MatchEngine::new();
        
        engine.create_orderbook(Symbol::from("DOGE"));
        assert!(engine.get_orderbook(&Symbol::from("DOGE")).is_some());
        assert_eq!(engine.symbols().count(), 1);
        
        // Should be empty initially
        let doge_book = engine.get_orderbook(&Symbol::from("DOGE")).unwrap();
        assert!(doge_book.best_price(&Side::Ask).is_none());
        assert!(doge_book.best_price(&Side::Bid).is_none());
    }
}
