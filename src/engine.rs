use crate::{order, transaction};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExecutionError {
    AddOrder(Box<crate::book::AddOrderError>),
    UnknownSymbol(order::Symbol),
}

impl From<crate::book::AddOrderError> for ExecutionError {
    fn from(value: crate::book::AddOrderError) -> Self {
        Self::AddOrder(Box::new(value))
    }
}

/// A match engine tracks multiple orderbooks, one per symbol.
pub struct MatchEngine {
    orderbooks: HashMap<order::Symbol, crate::book::Book>,
}

impl MatchEngine {
    pub fn new() -> Self {
        Self {
            orderbooks: HashMap::new(),
        }
    }

    pub fn add_orderbook(&mut self, symbol: order::Symbol) {
        self.orderbooks.insert(symbol, crate::book::Book::new(symbol));
    }

    pub fn add_order(&mut self, order: order::Order) -> Result<transaction::Log, ExecutionError> {
        let mut log = transaction::Log::new();
        let symbol = *order.symbol();
        let book = self.orderbooks.get_mut(&symbol)
            .ok_or_else(|| ExecutionError::UnknownSymbol(symbol))?;
        book.execute(order, &mut log)?;
        Ok(log)
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
    use crate::order::{Id, Price, Quantity, Side, Symbol};
    use uuid::Uuid;

    #[test]
    fn rejects_orders_for_unknown_symbols() {
        let mut engine = MatchEngine::new();
        let btc_symbol = Symbol::new("BTCUSD").unwrap();
        let eth_symbol = Symbol::new("ETHUSD").unwrap();
        
        // Add orderbook for BTC only
        engine.add_orderbook(btc_symbol);
        
        // BTC order should succeed
        let btc_order = order::Order::builder()
            .id(Id::new(Uuid::new_v4()))
            .symbol(btc_symbol)
            .price(Price::new(50000))
            .quantity(Quantity::new(100))
            .side(Side::Ask)
            .build()
            .unwrap();
        
        assert!(engine.add_order(btc_order).is_ok());
        
        // ETH order should fail with UnknownSymbol
        let eth_order = order::Order::builder()
            .id(Id::new(Uuid::new_v4()))
            .symbol(eth_symbol)
            .price(Price::new(3000))
            .quantity(Quantity::new(100))
            .side(Side::Ask)
            .build()
            .unwrap();
        
        match engine.add_order(eth_order) {
            Err(ExecutionError::UnknownSymbol(symbol)) => assert_eq!(symbol, eth_symbol),
            _ => panic!("Expected UnknownSymbol error"),
        }
    }
}
