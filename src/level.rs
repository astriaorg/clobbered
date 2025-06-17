use std::cell::RefCell;
use std::ops::Deref;
use std::ops::DerefMut;

use crate::order;
use crate::order::Order;
use crate::order::Price;
use crate::order::Quantity;
use crate::order::Side;
use crate::transaction;

/// A price-level in an orderbook.
///
/// It's implemented using a flat vector of orders to be cache efficient. Orders
/// are removed as in a FIFO queue: older orders get priority over newer orders
/// and are popped first. To not trigger memcpy's on pop, cancel, or fill,
/// orders with zero volume are treated as holes (inspired by `Vec<Option<T>>`
/// or the crates `stable-vec` and `slotmap`, but much simpler).
///
/// # Changing order volume and cancelling orders
///
/// The only crate-level API to manipulate order volume
/// that is exposed at the crate level is  [`Level::orders_mut`].
/// It yields an [`Entry`] object through which an order's volume
/// can be manipulated. On drop, [`Entry`] will decrement a
/// level-internal counter tracking orders with volumes. It is critical
/// that this invariant is held up if new methods to manipulate order
/// quantity are introduced, because it is used to determine if a price-level
/// should be dropped from the orderbook.
#[derive(Debug)]
pub(crate) struct Level {
    /// The side that this level is on.
    side: Side,
    /// The price which this levels applies to.
    price: Price,
    /// A flat vector of orders to make iteration cache efficient.
    /// Orders are removed by setting their volume to zero. Volumes
    /// with zero volume are skipped and considered removed from the
    /// backing storage.
    ///
    /// Lookup and removal at the front are O(1). Lookup and removal
    /// are O(n) because the vector has to be iterated.
    inner: Vec<Order>,

    /// Tracks the number of orders that have volume. Uses a `RefCell` to allow
    /// mutable access to this counter when decrementing it during mutable
    /// iteration through orders.
    orders_with_volume: RefCell<usize>,
}

impl Level {
    pub(crate) fn new(price: Price, side: Side) -> Self {
        Self {
            side,
            price,
            // TODO: What's an optimal pre-allocated size for the deque?
            // Either way, we should probably think about using some kind of
            // arena anyway.
            inner: Vec::with_capacity(1024),
            orders_with_volume: RefCell::new(0),
        }
    }

    /// Returns if the level contains any orders.
    ///
    /// This is a cheap O(1) operation that checks an internal counter.
    pub(crate) fn has_volume(&self) -> bool {
        *self.orders_with_volume.borrow() != 0
    }

    /// Returns whether [`order`] crosses the level.
    ///
    /// The level is crossed if:
    ///
    /// + level.side is ask and level.price <= order.price,
    /// + level.side is bid and level.price >= order.price
    ///
    /// If `order` is a stop order, then order.stop_price is used instead.
    ///
    /// This function assumes that [`order`] has the opposite side of the level.
    /// If it does not then this method is meaningless.
    pub(crate) fn is_crossed_by_order(&self, order: &Order) -> bool {
        debug_assert_eq!(self.side, order.side.opposite());
        let order_price = if order.is_stop() {
            order.stop_price()
        } else {
            order.price()
        };
        match &self.side {
            Side::Ask => &self.price <= order_price,
            Side::Bid => &self.price >= order_price,
        }
    }

    /// Pushes a new order to the back of the level.
    pub(crate) fn push(&mut self, order: Order) {
        debug_assert_eq!(&self.side, order.side());
        if order.has_volume() {
            self.orders_with_volume.replace_with(|old| {
                old.checked_add_signed(1).expect(
                    "orders-with-volume counter overflowed; this would require usize::MAX \
                        orders to be in the level, which in practice should never happen",
                )
            });
        }
        self.inner.push(order);
    }

    /// Returns the order identified by `id` if it exists at this level.
    pub(crate) fn get(&self, id: &order::Id) -> Option<&Order> {
        self.orders().find(|order| order.id() == id)
    }

    /// Cancels the order identified by `id`.
    pub(crate) fn cancel(&mut self, id: &order::Id) {
        for mut order in self.orders_mut() {
            if order.id() == id {
                order.quantity = Quantity::zero();
                return;
            }
        }
    }

    /// Matches a taker [`order`] to the orders at this price level.
    ///
    /// Returns a transaction log of the executions performed.
    pub(crate) fn match_order<FOnRemove>(
        &mut self,
        taker: &mut Order,
        log: &mut transaction::Log,
        mut on_remove: FOnRemove,
    ) where
        FOnRemove: FnMut(&Order),
    {
        let level_price = self.price;
        'match_loop: for mut maker in &mut self.orders_mut() {
            let mut quantity_exchanged = Quantity::zero();
            if taker.quantity() >= maker.quantity() {
                quantity_exchanged = *maker.quantity();
                taker.quantity = taker.quantity().saturating_sub(&quantity_exchanged);
                maker.quantity = Quantity::zero();
            } else if !maker.needs_full_execution() {
                quantity_exchanged = *taker.quantity();
                maker.quantity = maker.quantity().saturating_sub(&quantity_exchanged);
                taker.quantity = Quantity::zero();
            }

            // A match only happened if the amount exchanged isn't zero. This covers the
            // case where the maker needs full execution, i.e. all-or-none.
            if !quantity_exchanged.is_zero() {
                log.push(transaction::Match {
                    active_order_id: *taker.id(),
                    passive_order_id: *maker.id(),
                    price: level_price,
                    quantity: quantity_exchanged,
                });
            }

            if maker.is_filled() {
                on_remove(&maker);
                log.push(transaction::Fill {
                    id: *maker.id(),
                    side: *maker.side(),
                    unfilled_quantity: Quantity::zero(),
                });
            }

            if taker.is_filled() {
                break 'match_loop;
            }
        }
    }

    /// Returns an iterator over the orders at this level.
    pub(crate) fn orders(&self) -> Orders<'_> {
        Orders {
            iter: self.inner.iter(),
        }
    }

    /// Returns an iterator over the mutable orders at this level.
    ///
    /// This iterator ensures that orders with their volume entirely
    /// removed are treated as a hole. A followup call to [`Self::orders`],
    /// [`Self::orders_mut`], or [`Self::drain`] will not yield them,
    /// and [`Self::get`] with their IDs will return `None`.
    ///
    /// Call [`Self::make_compact`] to remove the holes from the backing
    /// storage.
    pub(crate) fn orders_mut(&mut self) -> OrdersMut<'_> {
        OrdersMut {
            iter: self.inner.iter_mut(),
            orders_with_volume: &self.orders_with_volume,
        }
    }

    /// Drains the orders from this level, leaving it empty.
    pub(crate) fn drain_orders(&mut self) -> impl Iterator<Item = Order> {
        self.inner.drain(..).filter(|order| order.has_volume())
    }

    /// Clears all holes in the level, removing them from the backing storage.
    ///
    /// Holes are orders with all their volume removed.
    pub(crate) fn make_compact(&mut self) {
        self.inner.retain(|order| order.has_volume())
    }
}

/// An entry in the level.
pub(crate) struct Entry<'level> {
    order: &'level mut Order,
    orders_with_volume: &'level RefCell<usize>,
}

impl<'level> Deref for Entry<'level> {
    type Target = Order;
    fn deref(&self) -> &Self::Target {
        self.order
    }
}

impl<'level> DerefMut for Entry<'level> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.order
    }
}

impl<'level> Drop for Entry<'level> {
    fn drop(&mut self) {
        if !self.order.has_volume() {
            self.orders_with_volume
                .replace_with(|old| old.saturating_sub(1));
        }
    }
}

pub(crate) struct Orders<'level> {
    iter: std::slice::Iter<'level, Order>,
}

impl<'level> Iterator for Orders<'level> {
    type Item = &'level Order;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find(|order| order.has_volume())
    }
}

pub(crate) struct OrdersMut<'level> {
    iter: std::slice::IterMut<'level, Order>,
    orders_with_volume: &'level RefCell<usize>,
}

impl<'level> Iterator for OrdersMut<'level> {
    type Item = Entry<'level>;

    fn next(&mut self) -> Option<Self::Item> {
        for order in &mut self.iter {
            if order.has_volume() {
                return Some(Entry {
                    order,
                    orders_with_volume: self.orders_with_volume,
                });
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Level;
    use crate::order::{Id, Order, Price, Quantity, Side, Symbol};

    fn order() -> Order {
        Order::builder()
            .symbol(Symbol::new("BTCUSD").unwrap())
            .quantity(Quantity::new(10))
            .price(Price::new(5))
            .side(Side::Ask)
            .build()
            .unwrap()
    }

    #[test]
    fn level_has_volume_after_pushing_order() {
        let mut level = Level::new(Price::new(5), Side::Ask);
        level.push(order());
        assert!(level.has_volume());
    }

    #[test]
    fn level_has_no_volume_after_pushing_empty_order() {
        let mut level = Level::new(Price::new(5), Side::Ask);
        level.push(Order {
            quantity: Quantity::new(0),
            ..order()
        });
        assert!(!level.has_volume());
    }

    #[test]
    fn level_has_no_volume_after_cancelling_order() {
        let mut level = Level::new(Price::new(5), Side::Ask);
        let id = Id::new(uuid::Uuid::new_v4());
        level.push(Order { id, ..order() });
        level.cancel(&id);
        assert!(!level.has_volume());
    }

    #[test]
    fn updating_volume_during_iteration_works() {
        let mut level = Level::new(Price::new(5), Side::Ask);
        level.push(Order {
            id: Id::new(uuid::Uuid::new_v4()),
            ..order()
        });
        let known_id = Id::new(uuid::Uuid::new_v4());
        level.push(Order {
            id: known_id,
            ..order()
        });
        level.push(Order {
            id: Id::new(uuid::Uuid::new_v4()),
            ..order()
        });
        crate::assert_some!(level.get(&known_id));
        level.orders_mut().for_each(|mut order| {
            if order.id() == &known_id {
                order.quantity = Quantity::new(42);
            }
        });
        assert_eq!(
            Some(Quantity::new(42)),
            level.get(&known_id).map(|order| *order.quantity())
        );
    }

    #[test]
    fn removing_volume_during_iteration_deletes_order() {
        let mut level = Level::new(Price::new(5), Side::Ask);
        level.push(Order {
            id: Id::new(uuid::Uuid::new_v4()),
            ..order()
        });
        let known_id = Id::new(uuid::Uuid::new_v4());
        level.push(Order {
            id: known_id,
            ..order()
        });
        level.push(Order {
            id: Id::new(uuid::Uuid::new_v4()),
            ..order()
        });
        crate::assert_some!(level.get(&known_id));
        level.orders_mut().for_each(|mut order| {
            if order.id() == &known_id {
                order.quantity = Quantity::zero();
            }
        });
        assert_eq!(None, level.get(&known_id));
    }
}
