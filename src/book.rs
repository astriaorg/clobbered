use std::collections::{BTreeMap, HashMap, VecDeque};

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

    /// Pushes a new order to the back of the level.
    fn push_order(&mut self, order: order::Order) {
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
        let price = self.price;

        let mut quantity = order::Quantity::zero();

        'match_loop: while let Some(mut maker) = self.inner.pop_front() {
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
                    price,
                    quantity,
                });
            }
            if maker.is_filled() {
                log.push(transaction::Event::MakerFilled(transaction::MakerFilled {
                    id: *maker.id(),
                    side: *maker.side(),
                }))
            } else {
                self.inner.push_front(maker);
            }
            if taker.is_filled() {
                break 'match_loop;
            }
        }
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

    // Returns the best price stored in the halfbook.
    //
    // Depending on how it's parameterized, this is either the highest ask price or the lowest bid price.
    fn worst_price(&self) -> Option<&Price> {
        self.levels
            .last_key_value()
            .map(|(price, _)| price.as_ref())
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord,
{
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
        level.push_order(order);
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
            if let transaction::Event::MakerFilled(transaction::MakerFilled { id, .. }) = event {
                let _old = self.id_to_side_and_price.remove(id);
                crate::debug_assert_some!(_old);
            }
        }

        if order.is_filled() {
            log.push(transaction::Event::TakerFilled {
                id: *order.id(),
                side: *order.side(),
            });
        } else if !order.is_immediate() {
            log.push(transaction::Event::Added(order.clone()));
            self.add_order_unchecked(order);
        }
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
