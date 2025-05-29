pub mod book;
pub mod engine;
// pub mod execution;
pub mod order;
pub mod transaction;
// pub use book::Book;
// pub use order::Order;
// pub use order::Price;
// pub use order::Quantity;
// pub use order::Timestamp;
pub(crate) mod macros;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ok;

    // Test helper functions
    fn create_order_id() -> order::Id {
        order::Id::new()
    }

    fn create_limit_order(
        side: order::Side,
        price: u128,
        quantity: u128,
    ) -> order::Order {
        order::Order::new_limit(
            create_order_id(),
            side,
            order::Price::new(price),
            order::Quantity::new(quantity),
        )
    }

    fn create_market_order(
        side: order::Side,
        quantity: u128,
    ) -> order::Order {
        order::Order::new_market(
            create_order_id(),
            side,
            order::Quantity::new(quantity),
        )
    }

    #[test]
    fn test_order_creation() {
        let order = create_limit_order(order::Side::Bid, 100, 50);
        assert_eq!(order.side(), &order::Side::Bid);
        assert_eq!(*order.price(), order::Price::new(100));
        assert_eq!(*order.quantity(), order::Quantity::new(50));
        assert!(!order.is_filled());
        assert!(order.is_bid());
        assert!(!order.is_ask());
    }

    #[test]
    fn test_price_comparison() {
        let price1 = order::Price::new(100);
        let price2 = order::Price::new(110);
        
        // For asks, lower prices are better
        assert!(price1.is_better_than_or_equal_to(&price2, &order::Side::Ask));
        assert!(!price2.is_better_than_or_equal_to(&price1, &order::Side::Ask));
        
        // For bids, higher prices are better
        assert!(!price1.is_better_than_or_equal_to(&price2, &order::Side::Bid));
        assert!(price2.is_better_than_or_equal_to(&price1, &order::Side::Bid));
    }

    #[test]
    fn test_quantity_operations() {
        let q1 = order::Quantity::new(100);
        let q2 = order::Quantity::new(50);
        
        assert_eq!(q1.saturating_add(&q2), order::Quantity::new(150));
        assert_eq!(q1.saturating_sub(&q2), order::Quantity::new(50));
        assert_eq!(q2.saturating_sub(&q1), order::Quantity::zero());
        
        assert!(!q1.is_zero());
        assert!(order::Quantity::zero().is_zero());
    }

    #[test]
    fn test_side_operations() {
        assert_eq!(order::Side::Ask.opposite(), order::Side::Bid);
        assert_eq!(order::Side::Bid.opposite(), order::Side::Ask);
        assert!(order::Side::Ask.is_ask());
        assert!(order::Side::Bid.is_bid());
    }

    #[test]
    fn test_empty_book() {
        let book = book::Book::new();
        assert_eq!(book.best_price(&order::Side::Ask), None);
        assert_eq!(book.best_price(&order::Side::Bid), None);
    }

    #[test]
    fn test_add_single_order() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        let order = create_limit_order(order::Side::Bid, 100, 50);
        
        assert_ok!(book.add_order(order.clone(), &mut log));
        assert_eq!(book.best_price(&order::Side::Bid), Some(&order::Price::new(100)));
        assert!(book.contains(&order));
    }

    #[test]
    fn test_add_multiple_orders_same_side() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        let order1 = create_limit_order(order::Side::Bid, 100, 50);
        let order2 = create_limit_order(order::Side::Bid, 110, 30);
        let order3 = create_limit_order(order::Side::Bid, 90, 20);
        
        assert_ok!(book.add_order(order1, &mut log));
        assert_ok!(book.add_order(order2, &mut log));
        assert_ok!(book.add_order(order3, &mut log));
        
        // Best bid should be the highest price
        assert_eq!(book.best_price(&order::Side::Bid), Some(&order::Price::new(110)));
    }

    #[test]
    fn test_simple_match() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        // Add a bid order
        let bid_order = create_limit_order(order::Side::Bid, 100, 50);
        assert_ok!(book.add_order(bid_order.clone(), &mut log));
        
        // Add a matching ask order
        let ask_order = create_limit_order(order::Side::Ask, 100, 30);
        assert_ok!(book.add_order(ask_order.clone(), &mut log));
        
        // The ask should have been filled and removed
        assert!(!book.contains(&ask_order));
        // The bid should still be there with reduced quantity
        assert!(book.contains(&bid_order));
    }

    #[test]
    fn test_market_order_execution() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        // Add some limit orders to the book
        let ask1 = create_limit_order(order::Side::Ask, 100, 20);
        let ask2 = create_limit_order(order::Side::Ask, 105, 30);
        assert_ok!(book.add_order(ask1.clone(), &mut log));
        assert_ok!(book.add_order(ask2.clone(), &mut log));
        
        // Add a market buy order
        let market_order = create_market_order(order::Side::Bid, 40);
        assert_ok!(book.add_order(market_order, &mut log));
        
        // ask1 should be completely filled and removed
        // ask2 should be partially filled and still in book 
        assert!(!book.contains(&ask1)); // ask1 completely filled
        assert!(book.contains(&ask2));  // ask2 partially filled, still in book
    }

    #[test]
    fn test_order_cancellation() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        let order = create_limit_order(order::Side::Bid, 100, 50);
        let key = book::Key {
            side: *order.side(),
            id: *order.id(),
        };
        
        assert_ok!(book.add_order(order.clone(), &mut log));
        assert!(book.contains(&order));
        
        let cancelled = book.cancel_order(&key);
        assert!(cancelled.is_some());
        assert!(!book.contains(&order));
    }

    #[test]
    fn test_duplicate_order_id() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        let order1 = create_limit_order(order::Side::Bid, 100, 50);
        let order2 = order::Order::new_limit(
            *order1.id(), // Same ID
            order::Side::Ask,
            order::Price::new(105),
            order::Quantity::new(30),
        );
        
        assert_ok!(book.add_order(order1, &mut log));
        
        let result = book.add_order(order2, &mut log);
        assert!(matches!(result, Err(book::AddOrderError::IdAlreadyExists(_))));
    }

    #[test]
    fn test_match_engine() {
        let mut engine = engine::MatchEngine::new();
        
        let bid_order = create_limit_order(order::Side::Bid, 100, 50);
        let log = assert_ok!(engine.add_order(bid_order));
        
        // Should have one event (order added)
        assert_eq!(log.events().len(), 1);
    }

    #[test]
    fn test_order_types() {
        let limit_order = create_limit_order(order::Side::Bid, 100, 50);
        let market_order = create_market_order(order::Side::Bid, 50);
        
        assert!(!limit_order.is_market());
        assert!(market_order.is_market());
        assert!(market_order.is_immediate());
    }

    #[test]
    fn test_post_only_order() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        // Add an ask order
        let ask_order = create_limit_order(order::Side::Ask, 100, 50);
        assert_ok!(book.add_order(ask_order, &mut log));
        
        // Try to add a post-only bid that would cross
        let post_only_order = order::Order::new_post_only(
            create_order_id(),
            order::Side::Bid,
            order::Price::new(105), // Higher than best ask
            order::Quantity::new(30),
        );
        
        let result = book.add_order(post_only_order, &mut log);
        // Should succeed but not execute
        assert!(result.is_ok());
    }

    #[test]
    fn test_fill_or_kill_insufficient_liquidity() {
        let mut book = book::Book::new();
        let mut log = transaction::Log::new();
        
        // Add small ask order
        let ask_order = create_limit_order(order::Side::Ask, 100, 20);
        assert_ok!(book.add_order(ask_order, &mut log));
        
        // Try to add FOK order for more than available
        let fok_order = order::Order::new_fill_or_kill(
            create_order_id(),
            order::Side::Bid,
            order::Price::new(100),
            order::Quantity::new(50), // More than available
        );
        
        let result = book.add_order(fok_order, &mut log);
        assert!(matches!(result, Err(book::AddOrderError::InsufficientLiquidity { .. })));
    }
}