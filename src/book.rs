use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fmt::Pointer,
};

use crate::{
    order::{self, Price},
    transaction,
};

#[derive(Debug, PartialEq, Eq)]
pub enum AddOrderError {
    IdAlreadyExists(order::Order),
    /// Error to report insufficient liquidity for fill-or-kill orders.
    // TODO: should this be treated as an error or as a regular event?
    InsufficientLiquidity {
        taker_side: crate::order::Side,
        market_side: crate::order::Side,
        requested: crate::order::Quantity,
    },
    PostOnlyWouldCrossMarket(order::Order),
}

/// A price-level of the orderbook.
#[derive(Debug)]
struct Level {
    /// The side that this level is on.
    side: order::Side,
    /// The price which this levels applies to.
    price: Price,
    /// The deque is optimal for adding at the front and pushing
    /// at the back (so, the primary operations performed at a level).
    ///
    /// Removing a element from the middle of the deque is (i.e. element at index i): O(min(i, n-i)).
    /// This is why it *should* be better than a vec.
    ///
    /// However, it should be investigated what the actually best datastructure would be.
    ///
    /// TODO: investigate if this should be replaced by a BTreeSet
    /// or Vec or VecDeque (or even IndexMap). All depends on the optimal performance
    /// for the different operations.
    inner: VecDeque<order::Order>,
}

impl Level {
    fn new(price: Price, side: order::Side) -> Self {
        Self {
            side,
            price,
            // TODO: What's an optimal pre-allocated size for the deque?
            // Either way, we should probably think about using some kind of
            // arena anyway.
            inner: VecDeque::new(),
        }
    }

    fn price(&self) -> &Price {
        &self.price
    }

    /// Returns if the level has any makers.
    ///
    /// This function relies on empty orders being removed from the level.
    /// If orders with zero quantity are added to the level then this method
    /// will become meaningless.
    fn has_volume(&self) -> bool {
        !self.inner.is_empty()
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
    fn is_crossed_by_order(&self, order: &order::Order) -> bool {
        debug_assert_eq!(self.side, order.side.opposite());
        let order_price = if order.is_stop() {
            order.stop_price()
        } else {
            order.price()
        };
        match &self.side {
            order::Side::Ask => &self.price <= order_price,
            order::Side::Bid => &self.price >= order_price,
        }
    }

    fn pop(&mut self) -> Option<order::Order> {
        self.inner.pop_front()
    }

    /// Pushes a new order to the back of the level.
    fn push(&mut self, order: order::Order) {
        debug_assert_eq!(&self.side, order.side());
        self.inner.push_back(order);
    }

    fn remove_order(&mut self, id: &order::Id) -> Option<order::Order> {
        let idx = self.inner.iter().position(|order| &order.id == id)?;
        let order = self.inner.remove(idx);
        debug_assert!(order.is_some());
        order
    }

    /// Calculates the total volume at this level.
    ///
    /// Note that this quantity saturates at the maximum of the type
    /// underlying [`order::Quantity`]. It is assumed that [`Self::volume`]
    /// is used to check if there is enough volume available to fill
    /// an order. Since the order can only request up to [`order::Quantity`]
    /// saturating at its maximum is fine.
    fn calculate_volume(&self) -> order::Quantity {
        let (left, right) = self.inner.as_slices();
        left.iter()
            .fold(order::Quantity::zero(), |acc, order| {
                acc.saturating_add(order.quantity())
            })
            .saturating_add(&right.iter().fold(order::Quantity::zero(), |acc, order| {
                acc.saturating_add(order.quantity())
            }))
    }

    /// Matches a taker [`order`] to the orders at this price level.
    ///
    /// Returns a transaction log of the executions performed.
    fn match_order(&mut self, taker: &mut order::Order, log: &mut transaction::Log) {
        'match_loop: for maker in &mut self.inner {
            let mut quantity = order::Quantity::zero();
            if taker.quantity() >= maker.quantity() {
                quantity = taker.quantity().saturating_sub(maker.quantity());
                taker.quantity = taker.quantity().saturating_sub(&quantity);
                maker.quantity = order::Quantity::zero();
            } else if !maker.needs_full_execution() {
                quantity = maker.quantity().saturating_sub(maker.quantity());
                maker.quantity = maker.quantity().saturating_sub(&quantity);
                taker.quantity = order::Quantity::zero();
            }

            // A match only happened if the amount exchanged isn't zero. This covers the
            // case where the maker needs full execution, i.e. all-or-none.
            if !quantity.is_zero() {
                log.push(transaction::Event::Match {
                    taker_order_id: *taker.id(),
                    maker_order_id: *maker.id(),
                    price: self.price,
                    quantity,
                });
            }
            if taker.is_filled() {
                break 'match_loop;
            }
        }

        // The second pass over the level is needed to remove all orders with zero quantity.
        // Unfortunately this is necessary because an iterator needs to be stable during iteration.
        // With vec-deques there is no way to pop-check-push-next.
        self.inner.retain(|order| {
            if order.is_filled() {
                log.push(transaction::Event::MakerFilled(transaction::Filled {
                    id: *order.id(),
                    side: *order.side(),
                }));
                false
            } else {
                true
            }
        });
    }

    /// Returns an iterator over the orders at this level.
    fn orders(&self) -> impl Iterator<Item = &order::Order> {
        self.inner.iter()
    }
}
/// Ask prices. Used for ordering prices in ascending order.
#[derive(Debug, PartialEq, Eq)]
pub struct AskPrice(Price);

impl Ord for AskPrice {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // same ordering as the underlying price
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for AskPrice {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

/// Bid prices. Used for ordering prices in descending order.
#[derive(Debug, PartialEq, Eq)]
pub struct BidPrice(Price);

impl Ord for BidPrice {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // inverse ordering compared to the underlying price
        other.0.cmp(&self.0)
    }
}

impl PartialOrd for BidPrice {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // inverse ordering compared to the underlying price
        Some(other.0.cmp(&self.0))
    }
}

impl AsRef<Price> for AskPrice {
    fn as_ref(&self) -> &Price {
        &self.0
    }
}

impl From<Price> for AskPrice {
    fn from(val: Price) -> Self {
        Self(val)
    }
}

impl AsRef<Price> for BidPrice {
    fn as_ref(&self) -> &Price {
        &self.0
    }
}

impl From<Price> for BidPrice {
    fn from(val: Price) -> Self {
        Self(val)
    }
}

impl From<AskPrice> for Price {
    fn from(value: AskPrice) -> Self {
        value.0
    }
}

impl From<BidPrice> for Price {
    fn from(value: BidPrice) -> Self {
        value.0
    }
}

trait HasSide {
    fn side() -> order::Side;
}

impl HasSide for AskPrice {
    fn side() -> order::Side {
        order::Side::Ask
    }
}

impl HasSide for BidPrice {
    fn side() -> order::Side {
        order::Side::Bid
    }
}

/// One half of an orderbook.
///
/// This type is parameterized by the price (ask vs bid price),
/// because it affects the order of its levels (whether they are sorted
/// price-ascending for asks or price-descending for bids).
#[derive(Debug)]
struct Half<TPrice> {
    /// The side that this halfbook is for.
    side: order::Side,
    /// The price-time levels of the halfbook.
    ///
    /// Prices are indices into the map, with the
    /// levels themselves keeping a time-ordering.
    levels: BTreeMap<TPrice, Level>,
    /// Price-time levels of stop and stop-limit orders in the halfbook that
    /// are waiting to be activated.
    ///
    /// These are ordered by the price at which they would activate.
    stop_orders: BTreeMap<TPrice, Level>,
}

impl<TPrice> Half<TPrice>
where
    TPrice: HasSide,
{
    /// Constructs a new half-book.
    fn new() -> Self {
        Self {
            side: TPrice::side(),
            levels: BTreeMap::new(),
            stop_orders: BTreeMap::new(),
        }
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: Ord,
{
    /// Cancels an order identified by its `id` and returns it.
    ///
    /// Panics if the halfbook did not contain a level `price` or if
    /// the price level did not contain the an order with `id`.
    ///
    /// This method is supposed to only be called from [`Book`], which
    /// enforces these invariants.
    // TODO: Can we use borrow semantics to borrow P as TPrice instead?
    fn cancel<P: Into<TPrice>>(
        &mut self,
        id: &order::Id,
        type_: &order::Type,
        price: P,
    ) -> order::Order {
        let level = match type_ {
            order::Type::Limit => self
                .levels
                .get_mut(&price.into())
                .expect("invariant violated: `price` passed down from Book must map to a level"),
            order::Type::Stop | order::Type::StopLimit => self
                .stop_orders
                .get_mut(&price.into())
                .expect("invariant violated: `price` passed down from Book must map to a level"),

            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
        };
        level
            .remove_order(id)
            .expect("invariant violated: `id` passed down from Book must exist at this level")
    }

    fn drop_empty_levels(&mut self) {
        self.levels.retain(|_, level| level.has_volume());
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: AsRef<Price> + Ord,
{
    // Returns the best price stored in the halfbook.
    //
    // Depending on how it's parameterized, this is either the lowest ask price or the highest bid price.
    fn best_price(&self) -> Option<&Price> {
        self.levels
            .first_key_value()
            .map(|(price, _)| price.as_ref())
    }

    // Returns the best price level in the halfbook.
    //
    // Depending on how it's parameterized, this is either the lowest ask price or the highest bid price.
    fn best_price_level(&self) -> Option<&Level> {
        self.levels.first_key_value().map(|(_, level)| level)
    }

    fn iter_levels(&mut self) -> impl Iterator<Item = (&Price, &mut Level)> {
        self.levels
            .iter_mut()
            .map(|(price, level)| (price.as_ref(), level))
    }

    // Returns the best price stored in the halfbook.
    //
    // Depending on how it's parameterized, this is either the highest ask price or the lowest bid price.
    fn worst_price(&self) -> Option<&Price> {
        self.levels
            .last_key_value()
            .map(|(price, _)| price.as_ref())
    }
}

struct ActivateStopOrders<'book, TPrice> {
    side: order::Side,
    halfbook: &'book mut Half<TPrice>,
    draining_level: Option<Level>,
    reference_price: TPrice,
}

impl<'book, TPrice> ActivateStopOrders<'book, TPrice>
where
    TPrice: Ord,
{
    fn next_activating_order(&mut self) -> Option<order::Order> {
        loop {
            if let Some(order) = self.draining_level.as_mut().and_then(Level::pop) {
                return Some(order);
            };

            // XXX: remember that TPrice has different PartialOrd implementations depending on which
            // level we are at: ask prices are ordered in ascending order (like regular integers),
            // whiile bid prices are in descending order, which means that bid.price_1 < bid.price_2
            // evaluates to true bid.price_1 is *greater* than bid.price_2.
            //
            // NOTE: the ? operator is responsible for breaking loop (easily overlooked).
            if self.halfbook.stop_orders.first_key_value()?.0 <= &self.reference_price {
                self.draining_level = self
                    .halfbook
                    .stop_orders
                    .pop_first()
                    .map(|(_price, level)| level);
            }
        }
    }
}

impl<'book, TPrice> ActivateStopOrders<'book, TPrice>
where
    TPrice: From<Price> + Ord + std::fmt::Debug,
{
    /// Adds an order to the halfbook.
    ///
    /// Panics if an order with the same ID already exists in the halfbook.
    fn add_order_unchecked(&mut self, order: order::Order) {
        self.halfbook.add_order_unchecked(order);
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord,
{
    /// Starts the process of activating stop orders in the halfbook.
    fn start_activate_stop_orders(&mut self, price: &Price) -> ActivateStopOrders<'_, TPrice> {
        ActivateStopOrders {
            side: self.side,
            halfbook: self,
            draining_level: None,
            reference_price: TPrice::from(*price),
        }
    }

    fn has_enough_volume(&self, requested: &order::Quantity) -> bool {
        let mut vol = order::Quantity::zero();

        for level in self.levels.values() {
            vol = vol.saturating_add(&level.calculate_volume());
            // shortcircuit so that this iterator does not keep walking the underlying btree.
            //
            // TODO: check if this is actually cache efficient. might also need revisiting if
            // the underlying datastructure changes.
            if &vol >= requested {
                return true;
            }
        }
        &vol >= requested
    }

    /// Checks if the halfbook has enough volume at `price` or better to fill the `requested` amount.
    fn has_enough_volume_at_price_or_better(
        &self,
        price: &Price,
        requested: &order::Quantity,
    ) -> bool {
        let mut still_needed = *requested;
        for (_price, level) in self.levels.range(..=TPrice::from(*price)) {
            for order in level.orders() {
                if order.needs_full_execution() {
                    // Only consider the volume of this maker order if it could executed in full, skip otherwise.
                    if &still_needed >= order.quantity() {
                        still_needed = still_needed.saturating_sub(order.quantity());
                    }
                } else {
                    still_needed = still_needed.saturating_sub(order.quantity());
                }
                // short-circuit so that this iterator does not keep walking the full btree.
                if still_needed.is_zero() {
                    return true;
                }
            }
        }
        still_needed.is_zero()
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord + std::fmt::Debug,
{
    /// Adds an order to the halfbook.
    ///
    /// Panics if an order with the same ID already exists in the halfbook.
    fn add_order_unchecked(&mut self, order: order::Order) {
        // TODO: What are reasonable default sizes for the preallocated buffers?
        let level = match order.type_() {
            order::Type::Limit => self
                .levels
                .entry(TPrice::from(*order.price()))
                .or_insert_with(|| Level::new(*order.price(), self.side)),
            order::Type::Stop | order::Type::StopLimit => self
                .stop_orders
                .entry(TPrice::from(*order.stop_price()))
                .or_insert_with(|| Level::new(*order.stop_price(), self.side)),
            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
        };
        level.push(order);
    }
}

struct BestPricesIter__<'book, TPrice> {
    iter: std::collections::btree_map::IterMut<'book, TPrice, Level>,
}

impl<'book, TPrice> Iterator for BestPricesIter__<'book, TPrice>
where
    TPrice: AsRef<Price>,
{
    type Item = (&'book Price, &'book mut Level);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(t_price, level)| (t_price.as_ref(), level))
    }
}

/// An iterator over the best bid or ask prices.
///
/// This enum exists for 2 reasons:
///
/// 1. to keep iteration over the bid levels as an implementation detail, with [`BestPricesIter`]
///    the public interface.
/// 2. to keep the `AskPrice` and `BidPrice` out of the public interface, which are
///    parameterizing the ordering of the price-to-level map in the halfbooks.
enum AsksOrBidsBestPricesIter<'book> {
    Asks(BestPricesIter__<'book, AskPrice>),
    Bids(BestPricesIter__<'book, BidPrice>),
}

impl<'book> Iterator for AsksOrBidsBestPricesIter<'book> {
    type Item = (&'book Price, &'book mut Level);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AsksOrBidsBestPricesIter::Asks(best_prices_iter) => best_prices_iter.next(),
            AsksOrBidsBestPricesIter::Bids(best_prices_iter) => best_prices_iter.next(),
        }
    }
}

struct BestPricesIter<'book> {
    inner: AsksOrBidsBestPricesIter<'book>,
}

impl<'book> Iterator for BestPricesIter<'book> {
    type Item = (&'book Price, &'book mut Level);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// A price-time orderbook.
#[derive(Debug)]
pub struct Book {
    asks: Half<AskPrice>,
    bids: Half<BidPrice>,

    /// A map of order ID to its order side, type, and price.
    /// The side is to identify the correct halfbook, the type to
    /// get the corresponding level kind, and the price to get
    /// the correct price level itself.
    ///
    /// An entry in this map must correspond to an entry in
    /// the respective halfbook.
    ///
    /// TODO: Use a cheaper hasher. FxHash, rapidhash, GxHash?
    /// rapidhash contains a nice summary.
    id_to_side_and_price: HashMap<order::Id, (order::Side, order::Type, Price)>,
}

impl Book {
    /// Creates a new order book.
    pub fn new() -> Self {
        Self {
            asks: Half::new(),
            bids: Half::new(),
            id_to_side_and_price: HashMap::new(),
        }
    }

    /// Returns the best price of the [`side`] of the orderbook.
    ///
    /// That is either the lowest ask price or the highest bid price.
    pub fn best_price(&self, side: &order::Side) -> Option<&Price> {
        match side {
            order::Side::Ask => self.asks.best_price(),
            order::Side::Bid => self.bids.best_price(),
        }
    }

    /// Adds an incoming order to the order book, triggering a matching process.
    pub fn match_and_add(
        &mut self,
        mut order: order::Order,
        log: &mut transaction::Log,
    ) -> Result<(), AddOrderError> {
        if self.contains(order.id()) {
            return Err(AddOrderError::IdAlreadyExists(order));
        }

        let events_before = log.events.len();
        if self.does_order_cross_market(&order) {
            if order.is_post_only() {
                return Err(AddOrderError::PostOnlyWouldCrossMarket(order));
            }

            // if the order is a stop order and crosses the market, convert it to a market order.
            match order.type_ {
                order::Type::Stop => {
                    order.type_ = order::Type::Market;
                }
                order::Type::StopLimit => {
                    order.type_ = order::Type::Limit;
                }
                _ => {}
            }
        }

        if !order.is_post_only() && order.is_executable() {
            if order.is_market() {
                self.set_market_order_price(&mut order);
            }

            'match_block: {
                if order.needs_full_execution()
                    && !self.has_enough_volume_on_side_at_price_or_better(
                        &order.side().opposite(),
                        &order.price,
                        &order.quantity,
                    )
                {
                    // there are two types of orders that need full execution:
                    // 1. fill-or-kill,
                    // 2. all-or-none.
                    //
                    // Fill-or-kill are rejected immediately, all-or-none are
                    // put into the orderbook.

                    if order.is_fill_or_kill() {
                        return Err(AddOrderError::InsufficientLiquidity {
                            taker_side: *order.side(),
                            market_side: order.side().opposite(),
                            requested: *order.quantity(),
                        });
                    }

                    break 'match_block;
                }

                'level_loop: for (_current_price, level) in
                    self.iter_level_on_side(&order.side().opposite())
                {
                    if order.is_filled() || !level.is_crossed_by_order(&order) {
                        break 'level_loop;
                    }
                    level.match_order(&mut order, log);
                }

                self.drop_empty_levels(&order.side().opposite());
            }
        }

        // Clear those orders from the book that have been filled during the last match.
        for event in &log.events[events_before..] {
            if let transaction::Event::MakerFilled(transaction::Filled { id, .. }) = event {
                let _old = self.id_to_side_and_price.remove(id);
                crate::debug_assert_some!(_old);
            }
        }

        if order.is_filled() {
            log.push(transaction::Event::TakerFilled(transaction::Filled {
                id: *order.id(),
                side: *order.side(),
            }));
        } else if !order.is_immediate() {
            log.push(transaction::Event::Added(order.clone()));
            self.add_order_unchecked(order);
        }

        self.perform_full_match(log);

        Ok(())
    }

    /// Cancels an order identified by its `id`.
    ///
    /// Returns if an order was removed (i.e. if an order of `key` was known by the system).
    pub fn cancel_order(&mut self, id: &order::Id) -> Option<order::Order> {
        let (side, type_, price) = self.id_to_side_and_price.get(id)?;
        Some(match side {
            order::Side::Ask => self.asks.cancel(id, type_, *price),
            order::Side::Bid => self.bids.cancel(id, type_, *price),
        })
    }

    /// Returns if the order [`id`] is known by the orderbook.
    pub fn contains(&self, id: &order::Id) -> bool {
        self.id_to_side_and_price.contains_key(id)
    }

    /// Returns if [`order`] crosses the market.
    ///
    /// An ask order crosses the market if its ask price is lower than or equal to the bid price on the book.
    /// A bid order crosses the market if its bid price is lower than or equal to the ask price on the book.
    pub fn does_order_cross_market(&self, order: &order::Order) -> bool {
        match order.side() {
            order::Side::Ask => self.bids.best_price_level(),
            order::Side::Bid => self.asks.best_price_level(),
        }
        .is_some_and(|level| level.is_crossed_by_order(order))
    }

    /// Returns whether the [`side`] of the book has enough volume to satisfy the [`requested`] amount.
    pub fn has_enough_volume_on_side(
        &self,
        side: &order::Side,
        requested: &order::Quantity,
    ) -> bool {
        match side {
            order::Side::Ask => self.asks.has_enough_volume(requested),
            order::Side::Bid => self.bids.has_enough_volume(requested),
        }
    }

    /// Returns whether the [`side`] of the book has enough volume to satisfy the [`requested`] amount at [`price`] or better.
    pub fn has_enough_volume_on_side_at_price_or_better(
        &self,
        side: &order::Side,
        price: &order::Price,
        requested: &order::Quantity,
    ) -> bool {
        match side {
            order::Side::Ask => self
                .asks
                .has_enough_volume_at_price_or_better(price, requested),
            order::Side::Bid => self
                .bids
                .has_enough_volume_at_price_or_better(price, requested),
        }
    }

    /// Adds the order to the orderbook without performing any checks.
    ///
    /// This operation is risky because it can violate invariants (the order
    /// crossing the book, duplicate order IDs, zero quantity requested, and
    /// others).
    fn add_order_unchecked(&mut self, order: order::Order) {
        self.id_to_side_and_price
            .insert(*order.id(), (*order.side(), *order.type_(), *order.price()));
        match *order.side() {
            order::Side::Ask => self.asks.add_order_unchecked(order),
            order::Side::Bid => self.bids.add_order_unchecked(order),
        };
    }

    fn drop_empty_levels(&mut self, side: &order::Side) {
        match side {
            order::Side::Ask => self.asks.drop_empty_levels(),
            order::Side::Bid => self.bids.drop_empty_levels(),
        }
    }

    fn get_worst_price(&self, side: &order::Side) -> Option<&Price> {
        match side {
            order::Side::Ask => self.asks.worst_price(),
            order::Side::Bid => self.bids.worst_price(),
        }
    }

    fn iter_level_on_side(&mut self, side: &order::Side) -> BestPricesIter<'_> {
        match side {
            order::Side::Ask => BestPricesIter {
                inner: AsksOrBidsBestPricesIter::Asks(BestPricesIter__ {
                    iter: self.asks.levels.iter_mut(),
                }),
            },
            order::Side::Bid => BestPricesIter {
                inner: AsksOrBidsBestPricesIter::Bids(BestPricesIter__ {
                    iter: self.bids.levels.iter_mut(),
                }),
            },
        }
    }

    /// Activates all stop orders in the orderbook.
    ///
    /// Returns if orders have been activated.
    fn activate_stop_orders(&mut self, log: &mut crate::transaction::Log) {
        'activation: loop {
            let mut have_activations_happened_on_iteration = false;

            // activate bids
            //
            // TODO: there is a lot of duplication here. We should merge bid & ask activation, and also
            // the high Book::match_and_add.
            {
                let Some(best_ask_price) = self.asks.best_price().copied() else {
                    break 'activation;
                };
                let mut activation = self.bids.start_activate_stop_orders(&best_ask_price);

                while let Some(mut stop_order) = activation.next_activating_order() {
                    have_activations_happened_on_iteration = true;

                    let _ = self.id_to_side_and_price.remove(stop_order.id()).expect(
                        "invariant violated: a stop order at a level must be tracked by the book",
                    );
                    match stop_order.type_ {
                        order::Type::Stop => {
                            stop_order.type_ = order::Type::Market;
                            stop_order.price = match stop_order.slippage() {
                                Some(slippage) => best_ask_price.minus_slippage(slippage),
                                None => *self.asks.worst_price().expect("invariant violated: if there is best ask price, then there is a worst ask price"),
                            };
                        }
                        order::Type::StopLimit => {
                            stop_order.type_ = order::Type::Limit;
                        }
                        other => {
                            unimplemented!("only stop orders can be activated; got `{other:?}`")
                        }
                    }

                    let events_before = log.events.len();
                    if stop_order.needs_full_execution()
                        && !self.asks.has_enough_volume_at_price_or_better(
                            stop_order.price(),
                            stop_order.quantity(),
                        )
                    {
                        // TODO: stop execution here. In the high level match loop this generates an error, which we can't and don't want to do here..
                        // It should probably not produce an error and instead add a new event to the log.
                    } else {
                        'level_loop: for (_current_price, level) in self.asks.iter_levels() {
                            if stop_order.is_filled() || !level.is_crossed_by_order(&stop_order) {
                                break 'level_loop;
                            }
                            level.match_order(&mut stop_order, log);
                        }
                        self.asks.drop_empty_levels();
                    }

                    // Clear those orders from the book that have been filled during the last match.
                    for event in &log.events[events_before..] {
                        if let transaction::Event::MakerFilled(transaction::Filled { id, .. }) =
                            event
                        {
                            let _old = self.id_to_side_and_price.remove(id);
                            crate::debug_assert_some!(_old);
                        }
                    }
                    if stop_order.is_filled() {
                        log.push(transaction::Event::MakerFilled(transaction::Filled {
                            id: *stop_order.id(),
                            side: *stop_order.side(),
                        }));
                    } else if !stop_order.is_immediate() {
                        log.push(transaction::Event::Added(stop_order.clone()));
                        self.id_to_side_and_price.insert(*stop_order.id(), (*stop_order.side(), *stop_order.type_(), *stop_order.price()))
                            .expect("invariant violated: at the beginning of the loop we have removed this order from the book, and so it must not exist in the map");
                        activation.add_order_unchecked(stop_order);
                    }
                }
            }

            // activate asks
            {
                let Some(best_bid_price) = self.bids.best_price().copied() else {
                    break 'activation;
                };
                let mut activation = self.asks.start_activate_stop_orders(&best_bid_price);

                while let Some(mut stop_order) = activation.next_activating_order() {
                    have_activations_happened_on_iteration = true;

                    let _ = self.id_to_side_and_price.remove(stop_order.id()).expect(
                        "invariant violated: a stop order at a level must be tracked by the book",
                    );
                    match stop_order.type_ {
                        order::Type::Stop => {
                            stop_order.type_ = order::Type::Market;
                            stop_order.price = match stop_order.slippage() {
                                Some(slippage) => best_bid_price.plus_slippage(slippage),
                                None => *self.bids.worst_price().expect("invariant violated: if there is best ask price, then there is a worst ask price"),
                            };
                        }
                        order::Type::StopLimit => {
                            stop_order.type_ = order::Type::Limit;
                        }
                        other => {
                            unimplemented!("only stop orders can be activated; got `{other:?}`")
                        }
                    }

                    let events_before = log.events.len();
                    if stop_order.needs_full_execution()
                        && !self.bids.has_enough_volume_at_price_or_better(
                            stop_order.price(),
                            stop_order.quantity(),
                        )
                    {
                        // TODO: stop execution here. In the high level match loop this generates an error, which we can't and don't want to do here..
                        // It should probably not produce an error and instead add a new event to the log.
                    } else {
                        'level_loop: for (_current_price, level) in self.bids.iter_levels() {
                            if stop_order.is_filled() || !level.is_crossed_by_order(&stop_order) {
                                break 'level_loop;
                            }
                            level.match_order(&mut stop_order, log);
                        }
                        self.bids.drop_empty_levels();
                    }

                    // Clear those orders from the book that have been filled during the last match.
                    for event in &log.events[events_before..] {
                        if let transaction::Event::MakerFilled(transaction::Filled { id, .. }) =
                            event
                        {
                            let _old = self.id_to_side_and_price.remove(id);
                            crate::debug_assert_some!(_old);
                        }
                    }
                    if stop_order.is_filled() {
                        log.push(transaction::Event::TakerFilled(transaction::Filled {
                            id: *stop_order.id(),
                            side: *stop_order.side(),
                        }));
                    } else if !stop_order.is_immediate() {
                        log.push(transaction::Event::Added(stop_order.clone()));
                        self.id_to_side_and_price.insert(*stop_order.id(), (*stop_order.side(), *stop_order.type_(), *stop_order.price()))
                            .expect("invariant violated: at the beginning of the loop we have removed this order from the book, and so it must not exist in the map");
                        activation.add_order_unchecked(stop_order);
                    }
                }
            }

            if have_activations_happened_on_iteration {
                break 'activation;
            }
        }
    }

    /// Run a full match loop over the entire orderbook.
    ///
    /// After it ran, this method ensures that all orders on the book that crossed the
    /// market price are executed, and that stop orders that have crossed the market price are
    /// activated (and executed, if needed).
    fn perform_full_match(&mut self, log: &mut transaction::Log) {
        'full_match: loop {
            let mut match_happened = false;
            // first, execute all bids
            'match_bids: {
                let Some(best_ask_price) = self.asks.best_price().copied() else {
                    break 'match_bids;
                };
                'loop_bids: for (bid_price, bid_level) in &mut self.bids.levels {
                    if bid_price.as_ref() < &best_ask_price {
                        break 'loop_bids;
                    }
                    let events_before = log.events.len();
                    // match every single bid to all asks
                    for bid in &mut bid_level.inner {
                        'match_single_bid: for (_, ask_level) in self.asks.levels.iter_mut() {
                            if bid.is_filled() || ask_level.is_crossed_by_order(bid) {
                                break 'match_single_bid;
                            }
                            ask_level.match_order(bid, log);
                        }
                    }
                    // a second pass to remove filled bids is necessary because the iterator in the
                    // previous loop needs to be stable
                    bid_level.inner.retain(|order| {
                        if order.is_filled() {
                            log.push(transaction::Event::MakerFilled(transaction::Filled {
                                id: *order.id(),
                                side: *order.side(),
                            }));
                            false
                        } else {
                            true
                        }
                    });
                    for event in &log.events[events_before..] {
                        if let transaction::Event::MakerFilled(transaction::Filled { id, .. })
                        | transaction::Event::TakerFilled(transaction::Filled { id, .. }) = event
                        {
                            let _old = self.id_to_side_and_price.remove(id);
                            crate::debug_assert_some!(_old);
                        }
                    }
                    // We use the events logged as a proxy to determine if matches happened.
                    match_happened |= log.events.len() > events_before;
                }

                self.bids.drop_empty_levels();
            }

            // then, execute all asks
            // TODO: streamline both of these loops to avoid all that duplicate logic.
            'match_asks: {
                let Some(best_bid_price) = self.bids.best_price().copied() else {
                    break 'match_asks;
                };
                'loop_bids: for (ask_price, ask_level) in &mut self.asks.levels {
                    if ask_price.as_ref() > &best_bid_price {
                        break 'loop_bids;
                    }
                    let events_before = log.events.len();
                    // match every single bid to all asks
                    for ask in &mut ask_level.inner {
                        'match_single_ask: for (_, bid_level) in self.bids.levels.iter_mut() {
                            if ask.is_filled() || bid_level.is_crossed_by_order(ask) {
                                break 'match_single_ask;
                            }
                            bid_level.match_order(ask, log);
                        }
                    }
                    // a second pass to remove filled bids is necessary because the iterator in the
                    // previous loop needs to be stable
                    ask_level.inner.retain(|order| {
                        if order.is_filled() {
                            log.push(transaction::Event::MakerFilled(transaction::Filled {
                                id: *order.id(),
                                side: *order.side(),
                            }));
                            false
                        } else {
                            true
                        }
                    });
                    for event in &log.events[events_before..] {
                        if let transaction::Event::MakerFilled(transaction::Filled { id, .. })
                        | transaction::Event::TakerFilled(transaction::Filled { id, .. }) = event
                        {
                            let _old = self.id_to_side_and_price.remove(id);
                            crate::debug_assert_some!(_old);
                        }
                    }
                    // We use the events logged as a proxy to determine if matches happened.
                    match_happened |= log.events.len() > events_before;
                }

                self.bids.drop_empty_levels();
            }

            let events_before = log.events.len();
            self.activate_stop_orders(log);
            match_happened |= log.events.len() > events_before;

            if match_happened {
                break 'full_match;
            }
        }
    }

    /// Sets the price of the market order.
    ///
    /// If slippage is set, then the price will be set to the best ask/bid price
    /// in the orderbook plus/minus the slippage.
    ///
    /// If no slippage is set, then the price will be set to the worst ask/bid price
    /// (this means that when matching all price levels of the orderbook will be
    /// matched against).
    ///
    /// NOTE: if no price (best or worst) can be established for the order book the
    /// price will be left unchanged. Since
    fn set_market_order_price(&mut self, order: &mut order::Order) {
        // XXX: If there are no prices in the order book then the field is left unchanged.
        //
        // This does not matter because the subsequent and immediate match will yield no
        // matches.
        let market_price = match order.slippage() {
            Some(slippage) => {
                let Some(best_price) = self.best_price(&order.side().opposite()) else {
                    return;
                };
                match order.side() {
                    order::Side::Ask => best_price.plus_slippage(slippage),
                    order::Side::Bid => best_price.minus_slippage(slippage),
                }
            }
            None => {
                let Some(worst_price) = self.get_worst_price(&order.side().opposite()) else {
                    return;
                };
                *worst_price
            }
        };
        order.price = market_price;
    }
}

impl Default for Book {
    fn default() -> Self {
        Self::new()
    }
}
