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
}

impl Default for MatchEngine {
    fn default() -> Self {
        Self::new()
    }
}
