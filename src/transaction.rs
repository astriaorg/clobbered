use crate::order;

/// The sequence of transactions that was done to match an order.
pub struct Log {
    order_id: order::Id,
    pub(crate) events: Vec<Event>,
}

impl Log {
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
}

pub enum Event {
    /// An order that was added to the orderbook.
    Added(order::Order),
    /// An maker order that has been filled and is subsequently removed from the orderbook.
    MakerFilled(MakerFilled),
    /// A taker order that has been filled. These never make it to the orderbook.
    // TODO: it might make sense to keep this in the `Log`?
    TakerFilled { id: order::Id, side: order::Side },
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

pub struct MakerFilled {
    pub id: order::Id,
    pub side: order::Side,
}
