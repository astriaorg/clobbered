use crate::order;

/// The sequence of transactions that was done to match an order.
pub struct Log {
    order_id: order::Id,
    pub(crate) events: Vec<Event>,
}

impl Log {
    pub(crate) fn for_id(id: &order::Id) -> Self {
        Self {
            order_id: *id,
            // TODO: What's a good default for this?
            events: vec![],
        }
    }

    pub(crate) fn for_order(order: &order::Order) -> Self {
        Self::for_id(order.id())
    }

    pub(crate) fn push(&mut self, event: Event) {
        self.events.push(event);
    }
}

pub enum Event {
    /// An order that was added to the orderbook.
    Added(order::Order),
    /// Orders that have been cancelled.
    Cancelled { id: order::Id },
    /// An order that has been fully executed and, if it was on the orderbook, removed from the orderbook.
    Remove {
        id: order::Id,
        side: order::Side,
        unfilled_quantity: order::Quantity,
    },
    Match {
        /// The ID of the order that triggered a match, i.e. the order that is being filled by
        /// the passive order.
        active_order_id: crate::order::Id,
        /// The ID of the order in the orderbook that is filling the active order.
        passive_order_id: crate::order::Id,
        /// The price at which the transaction occured.
        price: crate::order::Price,
        /// The quantity that was exchanged.
        quantity: crate::order::Quantity,
    },
}
