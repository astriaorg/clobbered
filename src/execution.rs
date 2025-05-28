/// An execution is the result of the matching engine executing an incoming order.
pub struct Execution {
    /// The ID of the order that triggered the match.
    order_id: crate::order::Id,
    /// The list of transactions that make up the execution. These are the
    /// active and passive order pairs - i.e. the order that triggered the
    /// match and the order that was on the book.
    transactions: Vec<crate::transaction::Transaction>,
    /// The remaining quantity of the order the remains after the matching process.
    remaining_quantity: crate::order::Quantity,
    /// The total quantity executed across all transactions.
    quantity_executed: crate::order::Quantity,
    /// Fully filled orders to be removed from the orderbook.
    filled_orders: Vec<crate::order::Id>,
}

impl Execution {
    fn for_order(order: &crate::order::Order) -> Self {
        Self {
            order_id: *order.id(),
            transactions: vec![],
            remaining_quantity: crate::order::Quantity::zero(),
            quantity_executed: crate::order::Quantity::zero(),
            filled_orders: vec![],
        }
    }
}
