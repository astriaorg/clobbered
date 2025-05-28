use crate::order;

/// The sequence of transactions that was done to match an order.
pub struct Log {
    order_id: order::Id,
    events: Vec<Event>,
}

impl Log {
    pub fn new() -> Self {
        Self {
            order_id: order::Id::new(),
            events: vec![],
        }
    }

    pub(crate) fn for_order(order: &order::Order) -> Self {
        Self {
            order_id: *order.id(),
            // TODO: What's a good default for this?
            events: vec![],
        }
    }

    pub(crate) fn push(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn events(&self) -> &[Event] {
        &self.events
    }
}

#[derive(Debug)]
pub enum Event {
    /// An order that was added to the orderbook.
    Added(order::Order),
    /// An order that has been filled and is subsequently removed from the orderbook.
    Fill { id: order::Id, side: order::Side },
    Match {
        /// The ID of the order that triggered a match, i.e. the order that is being filled by
        /// the passive order.
        taker_order_id: crate::order::Id,
        /// The ID of the order in the orderbook that is filling the active order.
        maker_order_id: crate::order::Id,
        /// The price at which the transaction occured.
        price: crate::order::Price,
        /// The quantity that was exchanged.
        quantity: crate::order::Quantity,
    },
}
