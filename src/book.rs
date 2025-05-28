use std::collections::{BTreeMap, HashMap, VecDeque};

use crate::{
    order::{self, Price},
    transaction,
};

#[derive(Debug, PartialEq, Eq)]
pub enum AddOrderError {
    IdAlreadyExists(order::Order),
    InsufficientLiquidity {
        taker_side: crate::order::Side,
        market_side: crate::order::Side,
        requested: crate::order::Quantity,
    },
    PostOnlyWouldCrossMarket(order::Order),
}

/// A key uniquely identifying an order.
///
/// Contains its order ID, whether it's buy or sell side,
/// and its symbol.
pub struct Key {
    pub side: order::Side,
    pub id: order::Id,
}

/// A price-level of the orderbook.
#[derive(Debug)]
pub struct Level {
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

    pub fn price(&self) -> &Price {
        &self.price
    }

    fn has_volume(&self) -> bool {
        !self.inner.is_empty()
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
    pub fn calculate_volume(&self) -> order::Quantity {
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
    pub fn match_order(&mut self, taker: &mut order::Order, log: &mut transaction::Log) {
        let price = self.price;

        let mut quantity;

        'match_loop: while let Some(mut maker) = self.inner.pop_front() {
            if taker.quantity() > maker.quantity() {
                quantity = taker.quantity().saturating_sub(maker.quantity());
                taker.quantity = taker.quantity().saturating_sub(&quantity);
                maker.quantity = order::Quantity::zero();
            } else {
                quantity = maker.quantity().saturating_sub(maker.quantity());
                maker.quantity = maker.quantity().saturating_sub(&quantity);
                taker.quantity = order::Quantity::zero();
            }
            log.push(transaction::Event::Match {
                taker_order_id: *taker.id(),
                maker_order_id: *maker.id(),
                price,
                quantity,
            });
            if maker.is_filled() {
                log.push(transaction::Event::Fill {
                    id: *maker.id(),
                    side: *maker.side(),
                })
            } else {
                self.inner.push_front(maker);
            }
            if taker.is_filled() {
                break 'match_loop;
            }
        }
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
    /// levels expected to keep a time-ordering.
    levels: BTreeMap<TPrice, Level>,

    /// A map of order ID to its position in the level.
    ///
    /// TODO: Use a cheaper hasher. FxHash, rapidhash, GxHash?
    /// rapidhash contains a nice summary.
    id_to_position: HashMap<order::Id, TPrice>,
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
            id_to_position: HashMap::new(),
        }
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: Ord,
{
    /// Cancels an order identified by its `id`.
    ///
    /// Returns the order if it was present in the halfbook.
    fn cancel(&mut self, id: &order::Id) -> Option<order::Order> {
        let price = self.id_to_position.get(id)?;
        let level = self
            .levels
            .get_mut(price)
            .expect("invariant violated: an entry in id_to_position must map to a level");
        level.remove_order(id)
    }

    /// Returns if ID is known by the halfbook.
    fn contains(&self, id: &order::Id) -> bool {
        self.id_to_position.contains_key(id)
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

    /// Returns the total quantity in the halfbook at `price` or better (less or more, depengin on ask or bid).
    fn has_enough_volume_at_price_or_better(
        &self,
        price: &Price,
        requested: &order::Quantity,
    ) -> bool {
        let mut vol = order::Quantity::zero();
        for (_price, level) in self.levels.range(..=TPrice::from(*price)) {
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
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord + std::fmt::Debug,
{
    /// Adds an order to the halfbook.
    ///
    /// Panics if an order with the same ID already exists in the halfbook.
    fn add_order_unchecked(&mut self, order: order::Order) {
        assert_eq!(
            self.id_to_position
                .insert(*order.id(), TPrice::from(*order.price())),
            None
        );
        // TODO: What are reasonable default sizes for the preallocated buffers?
        let level = self
            .levels
            .entry(TPrice::from(*order.price()))
            .or_insert_with(|| Level::new(*order.price(), self.side));
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
enum AsksOrBidsBestPricesIter<'guard> {
    Asks(BestPricesIter__<'guard, AskPrice>),
    Bids(BestPricesIter__<'guard, BidPrice>),
}

impl<'guard> Iterator for AsksOrBidsBestPricesIter<'guard> {
    type Item = (&'guard Price, &'guard mut Level);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AsksOrBidsBestPricesIter::Asks(best_prices_iter) => best_prices_iter.next(),
            AsksOrBidsBestPricesIter::Bids(best_prices_iter) => best_prices_iter.next(),
        }
    }
}

pub struct BestPricesIter<'guard> {
    inner: AsksOrBidsBestPricesIter<'guard>,
}

impl<'guard> Iterator for BestPricesIter<'guard> {
    type Item = (&'guard Price, &'guard mut Level);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// A price-time orderbook.
#[derive(Debug)]
pub struct Book {
    asks: Half<AskPrice>,
    bids: Half<BidPrice>,
}

impl Book {
    /// Creates a new order book.
    pub fn new() -> Self {
        Self {
            asks: Half::new(),
            bids: Half::new(),
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
    pub fn add_order(
        &mut self,
        mut order: order::Order,
        log: &mut transaction::Log,
    ) -> Result<(), AddOrderError> {
        if self.contains(&order) {
            return Err(AddOrderError::IdAlreadyExists(order));
        }

        if order.is_post_only() {
            if self.does_order_cross_market(&order) {
                return Err(AddOrderError::PostOnlyWouldCrossMarket(order));
            }
        } else {
            if order.is_market() {
                // Sets the market price to either the worst price on the book (so that
                // the order will be done against the entire book), or to the best price +/-
                // slippage if the order contains slippage.
                self.set_market_order_price(&mut order);
            }

            // XXX: Right now we don't permit orders in the book that require full execution.
            // This means that all-or-none and fill-or-kill orders are functionally equivalent.
            // The reason is that all-or-none orders are a headache both when matching and also
            // when short circuiting: checking if there is enough liquidity requires a full
            // execution, whereas actually executing requires either two passes over the data (
            // one to check if a full execution would be possible, and then to do the execution),
            // or alternatively a roll-back mechanism that undoes the transaction.
            if order.needs_full_execution()
                && !self.has_enough_volume_on_side_at_price_or_better(
                    &order.side().opposite(),
                    &order.price,
                    &order.quantity,
                )
            {
                return Err(AddOrderError::InsufficientLiquidity {
                    taker_side: *order.side(),
                    market_side: order.side().opposite(),
                    requested: *order.quantity(),
                });
            }

            'level_loop: for (current_price, level) in
                self.iter_best_prices_mut(&order.side().opposite())
            {
                if order.is_filled()
                    || current_price
                        .is_better_than_or_equal_to(order.price(), &order.side().opposite())
                {
                    break 'level_loop;
                }
                level.match_order(&mut order, log);
            }

            self.drop_empty_levels(&order.side().opposite());
        }

        if order.is_filled() {
            log.push(transaction::Event::Fill {
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
    pub fn cancel_order(&mut self, key: &Key) -> Option<order::Order> {
        match key.side {
            order::Side::Ask => self.asks.cancel(&key.id),
            order::Side::Bid => self.bids.cancel(&key.id),
        }
    }

    /// Returns if the order is known by the orderbook.
    ///
    /// This function checks if `order.id` exists in the orderbook.
    pub fn contains(&self, order: &order::Order) -> bool {
        match order.side() {
            order::Side::Ask => self.asks.contains(order.id()),
            order::Side::Bid => self.bids.contains(order.id()),
        }
    }

    /// Returns if [`order`] crosses the market.
    pub fn does_order_cross_market(&self, order: &order::Order) -> bool {
        match order.side() {
            order::Side::Ask => self
                .bids
                .best_price()
                .is_some_and(|best_price| best_price > order.price()),
            order::Side::Bid => self
                .asks
                .best_price()
                .is_some_and(|best_price| best_price < order.price()),
        }
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

    fn iter_best_prices_mut(&mut self, side: &order::Side) -> BestPricesIter<'_> {
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
