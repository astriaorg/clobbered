use crate::{order, transaction};

#[derive(Debug)]
pub enum ExecutionError {
    AddOrder(Box<crate::book::AddOrderError>),
}

impl From<crate::book::AddOrderError> for ExecutionError {
    fn from(value: crate::book::AddOrderError) -> Self {
        Self::AddOrder(Box::new(value))
    }
}

/// A match engine tracks a list of orderbooks.
///
/// For now, only one order book is tracked (the engine does not know about symbols).
pub struct MatchEngine {
    the_order_book: crate::book::Book,
}

impl MatchEngine {
    pub fn new() -> Self {
        Self {
            the_order_book: crate::book::Book::new(),
        }
    }

    pub fn add_order(&mut self, order: order::Order) -> Result<transaction::Log, ExecutionError> {
        let mut log = transaction::Log::for_order(&order);
        self.the_order_book.add_order(order, &mut log)?;
        Ok(log)
    }
    
    pub fn best_price(&self, side: &order::Side) -> Option<&order::Price> {
        self.the_order_book.best_price(side)
    }
    
    pub fn cancel_order(&mut self, key: &crate::book::Key) -> Option<order::Order> {
        self.the_order_book.cancel_order(key)
    }
    
    pub fn contains_order(&self, order: &order::Order) -> bool {
        self.the_order_book.contains(order)
    }
    
    pub fn all_orders(&self) -> (Vec<(order::Price, Vec<&order::Order>)>, Vec<(order::Price, Vec<&order::Order>)>) {
        self.the_order_book.all_orders()
    }
}

impl Default for MatchEngine {
    fn default() -> Self {
        Self::new()
    }
}
