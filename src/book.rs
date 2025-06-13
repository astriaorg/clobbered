use crate::{level::Level, order::Side, transaction::Event};
use std::collections::{BTreeMap, HashMap};

use crate::{
    order::{self, Price},
    transaction,
};

impl order::Order {
    fn reference_price(&self) -> &Price {
        if self.is_stop() {
            self.stop_price()
        } else {
            self.price()
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddOrderError {
    IdAlreadyExists(order::Order),
}

#[derive(Debug)]
struct PriceToLevel<TPrice> {
    side: Side,
    inner: BTreeMap<TPrice, Level>,
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: HasSide,
{
    fn new() -> Self {
        Self {
            side: TPrice::side(),
            inner: BTreeMap::new(),
        }
    }
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: Ord,
{
    /// Returns the best price contained in the level.
    ///
    /// Returns `None` if [`Self`] does not contain any
    /// levels with volume.
    fn best_price(&self) -> Option<&TPrice> {
        for (price, level) in &self.inner {
            if level.has_volume() {
                return Some(price);
            }
        }
        None
    }
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: HasSide + Ord,
    TPrice::OppositePrice: From<Price>,
{
    fn is_crossed_by(&self, order: &order::Order) -> bool {
        debug_assert_eq!(self.side, order.side().opposite());
        if let Some(best_price) = self.inner.first_key_value().map(|(price, _)| price) {
            let order_price = if order.is_stop() {
                order.stop_price()
            } else {
                order.price()
            };
            best_price.crosses(*order_price)
        } else {
            false
        }
    }
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: From<order::Price> + Ord,
{
    fn cancel_order(&mut self, id: &order::Id, price: &TPrice) {
        let level = self
            .inner
            .get_mut(price)
            .expect("invariant violated: `price` passed down from Half must map to a level");
        level.cancel(id);
    }

    /// Inserts an order in the price-to-level map.
    ///
    /// Panics if the order is a market order.
    fn insert_order(&mut self, order: order::Order) {
        debug_assert_eq!(self.side, order.side);

        let level = match order.type_() {
            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
            _all_others => self
                .inner
                .entry(TPrice::from(*order.reference_price()))
                .or_insert_with(|| Level::new(*order.reference_price(), self.side)),
        };
        level.push(order);
    }

    /// Returns an order identified by `id` at `price` level, if it exists.
    fn get_order(&self, id: &order::Id, price: &order::Price) -> Option<&order::Order> {
        self.inner
            .get(&TPrice::from(*price))
            .and_then(|level| level.get(id))
    }
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: Ord + HasSide,
    TPrice::OppositePrice: From<Price>,
{
    /// Checks if the halfbook has enough volume at `price` or better to fill the `requested` amount.
    fn has_enough_volume_at_price_or_better(
        &self,
        price: &order::Price,
        requested: &order::Quantity,
    ) -> bool {
        let mut still_needed = *requested;
        'levels: for (level_price, level) in self.levels() {
            if level_price.crosses(*price) {
                break 'levels;
            }
            for order in level.orders() {
                if order.needs_full_execution() {
                    if &still_needed >= order.quantity() {
                        still_needed = still_needed.saturating_sub(order.quantity());
                    }
                } else {
                    still_needed = still_needed.saturating_sub(order.quantity());
                }

                // shortcircuit so that this iterator does not keep walking the underlying btree.
                //
                // TODO: check if this is actually cache efficient. might also need revisiting if
                // the underlying datastructure changes.
                if still_needed.is_zero() {
                    break 'levels;
                }
            }
        }
        still_needed.is_zero()
    }
}

impl<TPrice> PriceToLevel<TPrice>
where
    TPrice: Ord,
{
    /// Clears levels of orders with zero volume and removes levels with no volume.
    fn clean_levels_and_drop_empty(&mut self) {
        self.inner.retain(|_, level| {
            // XXX: Only compact levels if they have volume. Otherwise just drop them directly.
            if level.has_volume() {
                level.make_compact();
            }
            level.has_volume()
        });
    }

    /// Checks if the halfbook has enough volume to fill the `requested` amount.
    fn has_enough_volume(&self, requested: &order::Quantity) -> bool {
        let mut still_needed = *requested;
        'levels: for (_price, level) in self.levels() {
            for order in level.orders() {
                if order.needs_full_execution() {
                    if &still_needed >= order.quantity() {
                        still_needed = still_needed.saturating_sub(order.quantity());
                    }
                } else {
                    still_needed = still_needed.saturating_sub(order.quantity());
                }

                // shortcircuit so that this iterator does not keep walking the underlying btree.
                //
                // TODO: check if this is actually cache efficient. might also need revisiting if
                // the underlying datastructure changes.
                if still_needed.is_zero() {
                    break 'levels;
                }
            }
        }
        still_needed.is_zero()
    }
    /// Creates an iterator over the levels of this map.
    ///
    /// The levels are from best to worst (ascending if ask, descending if bid).
    fn levels(&self) -> impl Iterator<Item = (&TPrice, &Level)> {
        self.inner.iter()
    }

    /// Creates an iterator over the mutable levels of the this map.
    ///
    /// The levels are from best to worst (ascending if ask, descending if bid).
    fn levels_mut(&mut self) -> impl Iterator<Item = (&TPrice, &mut Level)> {
        self.inner.iter_mut()
    }
}

/// Ask prices. Used for ordering prices in ascending order.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct AskPrice(Price);

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
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct BidPrice(Price);

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
    type OppositePrice: HasSide;

    fn side() -> Side;

    fn worst_possible() -> Self;

    fn crosses<P>(&self, opposite: P) -> bool
    where
        // TODO: this exists so that one does not need to explicitly convert Price -> Ask/BidPrice everywhere.
        // This will likely only be implemented as crosses::<AskPrice> and crosses::<BidPrice>, but there might
        // be a way to write this in a nicer way.
        Self::OppositePrice: From<P>;
}

impl HasSide for AskPrice {
    type OppositePrice = BidPrice;

    fn side() -> Side {
        Side::Ask
    }

    fn worst_possible() -> Self {
        Self::from(order::Price::max())
    }

    fn crosses<P>(&self, opposite: P) -> bool
    where
        P: Into<Self::OppositePrice>,
    {
        let opposite: Self::OppositePrice = opposite.into();
        self.0 <= opposite.0
    }
}

impl HasSide for BidPrice {
    type OppositePrice = AskPrice;

    fn side() -> Side {
        Side::Bid
    }

    fn worst_possible() -> Self {
        Self::from(order::Price::zero())
    }

    fn crosses<P>(&self, opposite: P) -> bool
    where
        P: Into<Self::OppositePrice>,
    {
        let opposite: Self::OppositePrice = opposite.into();
        self.0 >= opposite.0
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
    side: Side,
    /// The price-time levels of the halfbook.
    ///
    /// Prices are indices into the map, with the
    /// levels themselves keeping a time-ordering.
    levels: PriceToLevel<TPrice>,
    /// Price-time levels of stop and stop-limit orders in the halfbook that
    /// are waiting to be activated.
    ///
    /// These are ordered by the price at which they would activate.
    stop_orders: PriceToLevel<TPrice>,
}

impl<TPrice> Half<TPrice>
where
    TPrice: HasSide,
{
    /// Constructs a new half-book.
    fn new() -> Self {
        Self {
            side: TPrice::side(),
            levels: PriceToLevel::new(),
            stop_orders: PriceToLevel::new(),
        }
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord,
{
    /// Cancels an order identified by its `id` and returns it.
    ///
    /// Panics if the halfbook did not contain a level `price` or if
    /// the price level did not contain the an order with `id`.
    ///
    /// This method is supposed to only be called from [`Book`], which
    /// enforces these invariants.
    // TODO: Can we use borrow semantics to borrow P as TPrice instead?
    fn cancel(&mut self, id: &order::Id, type_: &order::Type, price: &order::Price) {
        match type_ {
            order::Type::Limit => self.levels.cancel_order(id, &TPrice::from(*price)),
            order::Type::Stop | order::Type::StopLimit => {
                self.stop_orders.cancel_order(id, &TPrice::from(*price))
            }

            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
        };
    }

    /// Returns an order identified by `id`, `type_`, and `price` if it exists in the halfbook.
    ///
    /// Panics if the `type_` provided is a market order (market orders must not be stored in the
    /// halfbook).
    fn get(
        &self,
        id: &order::Id,
        type_: &order::Type,
        price: &order::Price,
    ) -> Option<&order::Order> {
        match type_ {
            order::Type::Limit => self.levels.get_order(id, price),
            order::Type::Stop | order::Type::StopLimit => self.stop_orders.get_order(id, price),
            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
        }
    }

    fn drop_empty_levels(&mut self) {
        self.levels.clean_levels_and_drop_empty();
    }

    fn drop_empty_stop_order_levels(&mut self) {
        self.stop_orders.clean_levels_and_drop_empty();
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: AsRef<Price> + Copy + From<Price> + HasSide + Ord,
    TPrice::OppositePrice: From<Price>,
{
    /// Executes `order` against the book.
    ///
    /// If `order` is a stop order, then this method will
    /// also convert it to an executable order (market, limit, etc),
    /// and set its price to the market price.
    ///
    /// This will leave the halfbook in a "dirty" state, potentially
    /// containing levels with no volume.
    ///
    /// This method will generate events for all order that are affected
    /// by this process on its side (specifically match and removal).
    ///
    /// It will not generate events that only affect `order`.
    ///
    /// Run [`Half::drop_empty_levels`] to explicitly remove levels
    /// with no volume.
    fn perform_match<FOnRemove>(
        &mut self,
        order: &mut order::Order,
        log: &mut transaction::Log,
        mut on_remove: FOnRemove,
    ) where
        FOnRemove: FnMut(&order::Order),
    {
        debug_assert_eq!(self.side, order.side().opposite());

        // XXX: use the worst possible price here to line up the types.
        // In practice this won't do anything because if there is no
        // best price then there are no orders to match against.
        let mut market_price = *self
            .best_price()
            .copied()
            .unwrap_or_else(TPrice::worst_possible)
            .as_ref();

        // Performs the conversion of stop order to executable order.
        let was_stop = order.is_stop();
        match order.type_ {
            order::Type::Stop => {
                order.type_ = order::Type::Market;
                if order.has_slippage() {
                    market_price = *order.stop_price();
                } else {
                    market_price = *TPrice::worst_possible().as_ref();
                }
            }
            order::Type::StopLimit => {
                order.type_ = order::Type::Limit;
            }
            _ => {}
        }

        if was_stop {
            log.push(Event::Activate(order.clone()));
        }

        if order.is_market() {
            order.set_market_price_considering_slippage(&market_price);
        }

        if order.needs_full_execution()
            && !self.has_enough_volume_at_price_or_better(&order.price, &order.quantity)
        {
            return;
        }

        'level_loop: for (_current_price, level) in self.levels.levels_mut() {
            if order.is_filled() || !level.is_crossed_by_order(order) {
                break 'level_loop;
            }
            level.match_order(order, log, &mut on_remove);
        }
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: AsRef<Price> + Ord,
{
    // Returns the best price stored in the halfbook.
    //
    // Depending on how it's parameterized, this is either the lowest ask price or the highest bid price.
    fn best_price(&self) -> Option<&TPrice> {
        self.levels.best_price()
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: HasSide + Ord,
    TPrice::OppositePrice: From<Price>,
{
    fn is_crossed_by(&self, order: &order::Order) -> bool {
        debug_assert_eq!(self.side, order.side().opposite());
        self.levels.is_crossed_by(order)
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: Ord,
{
    /// Checks if the halfbook has enough volume to fill the `requested` amount.
    fn has_enough_volume(&self, requested: &order::Quantity) -> bool {
        self.levels.has_enough_volume(requested)
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: Ord + HasSide,
    TPrice::OppositePrice: From<Price>,
{
    /// Checks if the halfbook has enough volume at `price` or better to fill the `requested` amount.
    fn has_enough_volume_at_price_or_better(
        &self,
        price: &Price,
        requested: &order::Quantity,
    ) -> bool {
        self.levels
            .has_enough_volume_at_price_or_better(price, requested)
    }
}

impl<TPrice> Half<TPrice>
where
    TPrice: From<Price> + Ord,
{
    /// Adds an order to the halfbook.
    fn add_order_unchecked(&mut self, order: order::Order) {
        debug_assert_eq!(&self.side, order.side());

        // TODO: What are reasonable default sizes for the preallocated buffers?
        match order.type_() {
            order::Type::Limit => self.levels.insert_order(order),
            order::Type::Stop | order::Type::StopLimit => self.stop_orders.insert_order(order),
            order::Type::Market => {
                unimplemented!("market orders must not be stored in the halfbook")
            }
        };
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
    id_to_side_and_price: HashMap<order::Id, (Side, order::Type, Price)>,
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
    pub fn best_price(&self, side: &Side) -> Option<&Price> {
        match side {
            Side::Ask => self.asks.best_price().map(AsRef::as_ref),
            Side::Bid => self.bids.best_price().map(AsRef::as_ref),
        }
    }

    /// Executes an order against the order book.
    ///
    /// This method attempts to match the order against the book, and if it has
    /// unfilled volume, adds it to the book (if not a market or fill-or-kill order).
    pub fn execute(
        &mut self,
        mut order: order::Order,
        log: &mut transaction::Log,
    ) -> Result<(), AddOrderError> {
        if self.contains(order.id()) {
            return Err(AddOrderError::IdAlreadyExists(order));
        }

        if order.is_post_only() && self.does_order_cross_market(&order) {
            log.push(transaction::Fill {
                id: *order.id(),
                side: *order.side(),
                unfilled_quantity: *order.quantity(),
            });
            return Ok(());
        }

        // TODO: should stop orders actually be executed at this point already or should they
        // first be put in the book and then executed? If we attempted to execute them here
        // already, we'd need to check if they are activated inside `perform_match`.
        if !order.is_post_only() && order.is_executable() {
            let remove_fn = make_remove_fn(&mut self.id_to_side_and_price);

            match order.side() {
                Side::Ask => self.bids.perform_match(&mut order, log, remove_fn),
                Side::Bid => self.asks.perform_match(&mut order, log, remove_fn),
            }
        }

        if order.is_filled() || order.is_immediate() {
            log.push(transaction::Fill {
                id: *order.id(),
                side: *order.side(),
                unfilled_quantity: *order.quantity(),
            });
        } else if !order.is_immediate() {
            log.push(Event::Add(order.clone()));
            self.add_order_unchecked(order);
        }

        self.perform_full_match(log);

        Ok(())
    }

    /// Cancels an order identified by its `id`.
    ///
    /// Returns if an order was removed.
    pub fn cancel(&mut self, id: &order::Id, log: &mut transaction::Log) -> bool {
        let Some((side, type_, price)) = self.id_to_side_and_price.remove(id) else {
            return false;
        };
        match side {
            Side::Ask => self.asks.cancel(id, &type_, &price),
            Side::Bid => self.bids.cancel(id, &type_, &price),
        }
        log.push(transaction::Cancel { id: *id });
        true
    }

    /// Returns if the order [`id`] is known by the orderbook.
    pub fn contains(&self, id: &order::Id) -> bool {
        self.id_to_side_and_price.contains_key(id)
    }

    /// Returns the order identified by `id` if it exists in the book.
    pub fn get_order(&self, id: &order::Id) -> Option<&order::Order> {
        let (side, type_, price) = self.id_to_side_and_price.get(id)?;
        match side {
            Side::Ask => self.asks.get(id, type_, price),
            Side::Bid => self.bids.get(id, type_, price),
        }
    }

    /// Returns the metadata for the order identified by `id` if it exists in the book.
    ///
    /// Right now, this is only tuple of [`Side`], `[Type]`, and [`Price`].
    pub fn get_order_meta(
        &self,
        id: &order::Id,
    ) -> Option<&(order::Side, order::Type, order::Price)> {
        self.id_to_side_and_price.get(id)
    }

    /// Returns if [`order`] crosses the market.
    ///
    /// An ask order crosses the market if its ask price is lower than or equal to the bid price on the book.
    /// A bid order crosses the market if its bid price is lower than or equal to the ask price on the book.
    pub fn does_order_cross_market(&self, order: &order::Order) -> bool {
        match order.side() {
            Side::Ask => self.bids.is_crossed_by(order),
            Side::Bid => self.asks.is_crossed_by(order),
        }
    }

    /// Returns whether the [`side`] of the book has enough volume to satisfy the [`requested`] amount.
    pub fn has_enough_volume_on_side(&self, side: &Side, requested: &order::Quantity) -> bool {
        match side {
            Side::Ask => self.asks.has_enough_volume(requested),
            Side::Bid => self.bids.has_enough_volume(requested),
        }
    }

    /// Returns whether the [`side`] of the book has enough volume to satisfy the [`requested`] amount at [`price`] or better.
    pub fn has_enough_volume_on_side_at_price_or_better(
        &self,
        side: &Side,
        price: &order::Price,
        requested: &order::Quantity,
    ) -> bool {
        match side {
            Side::Ask => self
                .asks
                .has_enough_volume_at_price_or_better(price, requested),
            Side::Bid => self
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
        self.id_to_side_and_price.insert(
            *order.id(),
            (*order.side(), *order.type_(), *order.reference_price()),
        );
        match *order.side() {
            Side::Ask => self.asks.add_order_unchecked(order),
            Side::Bid => self.bids.add_order_unchecked(order),
        };
    }

    /// Activates all stop orders in the orderbook.
    ///
    /// Returns if orders have been activated.
    fn activate_stop_orders(&mut self, log: &mut crate::transaction::Log) {
        // XXX: we will use these as the reference price to activate orders.
        //
        // This might very well mean that earlier stop-limit orders get filled
        // at their set price while later stop-limit orders won't get filled at all
        // but stil end up in the book as executable orders. But this seems to
        // be an intuitive mode of operation: the market did drop below a certain
        // price point but now does not have enough liquidity to accommodate the
        // stop-limit orders.
        let market_ask_price = self
            .asks
            .best_price()
            .copied()
            .unwrap_or_else(AskPrice::worst_possible);
        let market_bid_price = self
            .bids
            .best_price()
            .copied()
            .unwrap_or_else(BidPrice::worst_possible);

        'activate_all: loop {
            let mut have_activations_happened_on_iteration = false;

            'activate_bids: for (bid_price, stop_level) in self.bids.stop_orders.levels_mut() {
                if !bid_price.crosses(market_ask_price) {
                    break 'activate_bids;
                }

                for mut bid in stop_level.drain_orders() {
                    have_activations_happened_on_iteration = true;

                    self.asks.perform_match(
                        &mut bid,
                        log,
                        // XXX: need to construct the callback closure in the loop because we need to be able to insert the stop order into the map right after.
                        make_remove_fn(&mut self.id_to_side_and_price),
                    );

                    if bid.is_filled() || bid.is_immediate() {
                        let _ = self.id_to_side_and_price.remove(bid.id()).expect(
                            "invariant violated: a stop order at a level must be tracked by the book",
                        );
                        log.push(transaction::Fill {
                            id: *bid.id(),
                            side: *bid.side(),
                            unfilled_quantity: order::Quantity::zero(),
                        });
                    } else {
                        log.push(Event::Add(bid.clone()));
                        let item = self.id_to_side_and_price.get_mut(bid.id()).expect(
                            "invariant violated: a stop order at a level must be tracked by the book",
                        );
                        *item = (*bid.side(), *bid.type_(), *bid.price());
                        self.bids.levels.insert_order(bid);
                    }
                }
            }

            'activate_asks: for (ask_price, stop_level) in self.asks.stop_orders.levels_mut() {
                if !ask_price.crosses(market_bid_price) {
                    break 'activate_asks;
                }

                for mut ask in stop_level.drain_orders() {
                    have_activations_happened_on_iteration = true;

                    self.bids.perform_match(
                        &mut ask,
                        log,
                        // XXX: need to construct the callback closure in the loop because we need to be able to insert the stop order into the map right after.
                        &mut make_remove_fn(&mut self.id_to_side_and_price),
                    );

                    if ask.is_filled() || ask.is_immediate() {
                        let _ = self.id_to_side_and_price.remove(ask.id()).expect(
                            "invariant violated: a stop order at a level must be tracked by the book",
                        );
                        log.push(transaction::Fill {
                            id: *ask.id(),
                            side: *ask.side(),
                            unfilled_quantity: *ask.quantity(),
                        });
                    } else {
                        log.push(Event::Add(ask.clone()));
                        let item = self.id_to_side_and_price.get_mut(ask.id()).expect(
                            "invariant violated: a stop order at a level must be tracked by the book",
                        );
                        *item = (*ask.side(), *ask.type_(), *ask.price());
                        self.asks.levels.insert_order(ask);
                    }
                }
            }

            if !have_activations_happened_on_iteration {
                break 'activate_all;
            }
        }

        // XXX: Drop all the empty levels at the end of the activation process.
        //
        // This has the following effect:
        self.asks.drop_empty_levels();
        self.bids.drop_empty_levels();
        self.asks.drop_empty_stop_order_levels();
        self.bids.drop_empty_stop_order_levels();
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
                // XXX: Construct this closure here so we can pass it to the half-book matcher but
                // drop it before running the activate-stops logic.
                let mut remove_fn = make_remove_fn(&mut self.id_to_side_and_price);

                let Some(best_ask_price) = self.asks.best_price().copied() else {
                    break 'match_bids;
                };
                'loop_bids: for (bid_price, bid_level) in &mut self.bids.levels.levels_mut() {
                    if !bid_price.crosses(best_ask_price) {
                        break 'loop_bids;
                    }
                    let events_before = log.events.len();
                    // match every single bid to all asks
                    for mut bid in bid_level.orders_mut() {
                        self.asks.perform_match(&mut bid, log, &mut remove_fn);
                        if bid.is_filled() {
                            remove_fn(&mut bid);
                            log.push(transaction::Fill {
                                id: *bid.id(),
                                side: *bid.side(),
                                unfilled_quantity: order::Quantity::zero(),
                            });
                        }
                    }

                    // We use the events logged as a proxy to determine if matches happened.
                    match_happened |= log.events.len() > events_before;
                }
            }

            // then, execute all asks
            // TODO: streamline both of these loops to avoid all that duplicate logic.
            'match_asks: {
                // XXX: Construct this closure here so we can pass it to the half-book matcher but
                // drop it before running the activate-stop logic.
                let mut remove_fn = make_remove_fn(&mut self.id_to_side_and_price);

                let Some(best_bid_price) = self.bids.best_price().copied() else {
                    break 'match_asks;
                };
                'loop_bids: for (ask_price, ask_level) in self.asks.levels.levels_mut() {
                    if !ask_price.crosses(best_bid_price) {
                        break 'loop_bids;
                    }
                    let events_before = log.events.len();
                    // match every single bid to all asks
                    for mut ask in &mut ask_level.orders_mut() {
                        self.bids.perform_match(&mut ask, log, &mut remove_fn);
                        if ask.is_filled() {
                            remove_fn(&mut ask);
                            log.push(transaction::Fill {
                                id: *ask.id(),
                                side: *ask.side(),
                                unfilled_quantity: order::Quantity::zero(),
                            });
                        }
                    }

                    // We use the events logged as a proxy to determine if matches happened.
                    match_happened |= log.events.len() > events_before;
                }
            }

            let events_before = log.events.len();
            self.activate_stop_orders(log);
            match_happened |= log.events.len() > events_before;

            if !match_happened {
                break 'full_match;
            }
        }
    }
}

impl Default for Book {
    fn default() -> Self {
        Self::new()
    }
}

// utility to create a closure that is passed to the various matching functions to
// remove orders in the higher-level id-to-side-and-price map (to avoid needing a
// second pass over the data).
fn make_remove_fn(
    map: &mut HashMap<order::Id, (Side, order::Type, Price)>,
) -> impl FnMut(&order::Order) {
    |order| {
        let _old = map.remove(order.id());
        crate::debug_assert_some!(_old);
    }
}

#[cfg(test)]
mod tests {
    use uuid::Uuid;

    use super::{AskPrice, BidPrice, Book, HasSide, PriceToLevel};
    use crate::{
        order::{Id, Order, Price, Quantity, Side, Type},
        transaction::{self, Event, Fill, Log},
    };

    fn order() -> Order {
        Order::builder()
            .id(Id::new(Uuid::new_v4()))
            .quantity(Quantity::new(10))
            .price(Price::new(5))
            .side(Side::Ask)
            .build()
            .unwrap()
    }

    #[test]
    fn are_prices_crossing() {
        let five = Price::new(5);
        let ten = Price::new(10);
        assert!(!AskPrice::from(ten).crosses(five));
        assert!(!AskPrice::from(ten).crosses(BidPrice::from(five)));

        assert!(AskPrice::from(five).crosses(ten));
        assert!(AskPrice::from(five).crosses(BidPrice::from(ten)));

        assert!(!BidPrice::from(five).crosses(ten));
        assert!(!BidPrice::from(five).crosses(AskPrice::from(ten)));

        assert!(BidPrice::from(ten).crosses(five));
        assert!(BidPrice::from(ten).crosses(AskPrice::from(five)));
    }

    #[test]
    fn limit_order_is_added_to_empty_book() {
        // scenario: the orderbook is empty, a limit order comes in
        //
        // expected result: the limit order is added to the book.
        let mut book = Book::new();
        let id = Id::new(uuid::Uuid::new_v4());
        let price = Price::new(10);
        let side = Side::Bid;
        let type_ = Type::Limit;
        let order = Order {
            id,
            side,
            price,
            type_,
            ..order()
        };
        let mut logs = Log::new();
        book.execute(order, &mut logs).unwrap();
        logs.iter()
            .find(|event| event.is_add())
            .expect("there should be an event for adding the order to the book");
        assert_eq!(
            Some(&(side, type_, price)),
            book.get_order_meta(&id),
            "the order should exist in the book"
        );
        assert_eq!(
            Some(&price),
            book.best_price(&side),
            "the best bid price should be the price of the order just added",
        );
    }

    #[test]
    fn market_order_is_passed_through_empty_book() {
        // scenario: the orderbook is empty, a market order comes in
        //
        // expected result: the market order is passed through and left
        // completely unfilled.
        let mut book = Book::new();
        let id = Id::new(uuid::Uuid::new_v4());
        let side = Side::Bid;
        let type_ = Type::Market;
        let quantity = Quantity::new(42);
        let order = Order {
            id,
            side,
            type_,
            quantity,
            ..order()
        };
        let mut logs = Log::new();
        book.execute(order, &mut logs).unwrap();
        let fill = logs
            .iter()
            .find_map(|event| event.as_fill())
            .expect("there should be an event for adding the order to the book");
        assert_eq!(fill.unfilled_quantity, quantity);
        crate::assert_none!(logs.iter().find(|event| event.is_add()));

        crate::assert_none!(
            book.get_order_meta(&id),
            "a market order should not exist in the book",
        );
        crate::assert_none!(
            book.best_price(&side),
            "the book is still empty, there should not be a best price",
        );
    }

    #[test]
    fn market_order_is_partially_filled() {
        // scenario: an ask-limit order sits on the book, a bid market order comes in,
        // requesting more volume than available.
        //
        // expected result: the market order is partially filled at the price of the ask
        // order, the ask order limit order is completely filled.
        let mut book = Book::new();
        let ask_id = Id::new(uuid::Uuid::new_v4());
        let ask_quantity = Quantity::new(9);
        let ask_price = Price::new(5);
        let mut log = Log::new();
        crate::assert_ok!(book.execute(
            Order {
                id: ask_id,
                quantity: ask_quantity,
                price: ask_price,
                side: Side::Ask,
                ..order()
            },
            &mut log,
        ));
        let bid_id = Id::new(uuid::Uuid::new_v4());
        let bid_quantity = Quantity::new(20);
        crate::assert_ok!(book.execute(
            Order {
                id: bid_id,
                quantity: bid_quantity,
                type_: Type::Market,
                side: Side::Bid,
                ..order()
            },
            &mut log,
        ));
        let match_ = log.iter().find_map(|event| event.as_match()).unwrap();
        assert_eq!(
            &transaction::Match {
                active_order_id: bid_id,
                passive_order_id: ask_id,
                price: ask_price,
                quantity: ask_quantity
            },
            match_
        );
        let fill_ask = log
            .iter()
            .find_map(|event| {
                {
                    event
                        .as_fill()
                        .and_then(|fill| (fill.id == ask_id).then_some(fill))
                }
            })
            .unwrap();
        assert_eq!(
            &Fill {
                id: ask_id,
                side: Side::Ask,
                unfilled_quantity: Quantity::zero(),
            },
            fill_ask,
        );
        let fill_bid = log
            .iter()
            .find_map(|event| {
                {
                    event
                        .as_fill()
                        .and_then(|fill| (fill.id == bid_id).then_some(fill))
                }
            })
            .unwrap();
        assert_eq!(
            &Fill {
                id: bid_id,
                side: Side::Bid,
                unfilled_quantity: Quantity::new(11),
            },
            fill_bid,
        );
    }

    #[test]
    fn market_order_is_filled_by_two_orders() {
        // scenario: two ask-limit order sit on the book at different prices,
        // a bid market order comes in, requesting less than the total available volume,
        // but more than the volumes provided by each of the on-book orders.
        //
        // expected result: the market order is completely filled. The ask order
        // at the lower price is completey filled. The ask order at the higher price
        // is partially filled.
        let mut book = Book::new();

        let cheap_id = Id::new(uuid::Uuid::new_v4());
        let expensive_id = Id::new(uuid::Uuid::new_v4());

        let mut log = Log::new();
        crate::assert_ok!(book.execute(
            Order {
                id: cheap_id,
                quantity: Quantity::new(5),
                price: Price::new(5),
                side: Side::Ask,
                ..order()
            },
            &mut log,
        ));
        crate::assert_ok!(book.execute(
            Order {
                id: expensive_id,
                quantity: Quantity::new(5),
                price: Price::new(6),
                side: Side::Ask,
                ..order()
            },
            &mut log,
        ));
        let bid_id = Id::new(uuid::Uuid::new_v4());
        crate::assert_ok!(book.execute(
            Order {
                id: bid_id,
                quantity: Quantity::new(8),
                type_: Type::Market,
                side: Side::Bid,
                ..order()
            },
            &mut log,
        ));
        let cheap_match = log
            .iter()
            .find_map(|event| {
                event
                    .as_match()
                    .and_then(|match_| (match_.passive_order_id == cheap_id).then_some(match_))
            })
            .unwrap();
        assert_eq!(
            &transaction::Match {
                active_order_id: bid_id,
                passive_order_id: cheap_id,
                price: Price::new(5),
                quantity: Quantity::new(5),
            },
            cheap_match,
        );
        let expensive_match = log
            .iter()
            .find_map(|event| {
                event
                    .as_match()
                    .and_then(|match_| (match_.passive_order_id == expensive_id).then_some(match_))
            })
            .unwrap();
        assert_eq!(
            &transaction::Match {
                active_order_id: bid_id,
                passive_order_id: expensive_id,
                price: Price::new(6),
                quantity: Quantity::new(3),
            },
            expensive_match,
        );
    }

    #[test]
    fn stop_limit_order_is_added_to_book() {
        // scenario: the orderbook is empty, a stop order comes in
        //
        // expected result: the stop order should be tracked by the book,
        // but it should not be considered as executable/affect the price
        // of its side.
        let mut book = Book::new();
        let id = Id::new(uuid::Uuid::new_v4());
        let price = Price::new(10);
        let stop_price = Price::new(11);
        let side = Side::Bid;
        let type_ = Type::StopLimit;
        let order = Order {
            id,
            side,
            price,
            stop_price,
            type_,
            ..order()
        };
        let mut logs = Log::new();
        book.execute(order, &mut logs).unwrap();
        assert_eq!(
            Some(&(side, type_, stop_price)),
            book.get_order_meta(&id),
            "the orders should exist in the book"
        );
        crate::assert_none!(
            book.best_price(&side),
            "there should not be a best bid price because the order is a stop order "
        );
    }

    #[test]
    fn stop_limit_order_activates_on_limit_order() {
        // scenario: an ask stop limit order sits in the order book with a stop price lower
        // than its limit price. a limit order bid comes in at the same price as the stop price.
        //
        // expected result: the stop limit order becomes a limit order on the book. both orders now
        // sit in the book. No matches between the two orders happen.
        let mut book = Book::new();
        let id = Id::new(uuid::Uuid::new_v4());
        let mut log = Log::new();
        book.execute(
            Order {
                id,
                side: Side::Ask,
                type_: Type::StopLimit,
                price: Price::new(20),
                stop_price: Price::new(10),
                ..order()
            },
            &mut log,
        )
        .unwrap();
        book.execute(
            Order {
                price: Price::new(15),
                side: Side::Bid,
                ..order()
            },
            &mut log,
        )
        .unwrap();
        crate::assert_some!(book.get_order(&id));
        crate::assert_none!(
            log.iter().find(|event| event.is_match()),
            "no matches should have occured"
        );
        assert_eq!(Some(&Price::new(15)), book.best_price(&Side::Bid));
    }

    #[test]
    fn price_level_with_empty_levels_does_not_have_best_price() {
        let mut price_to_levels = PriceToLevel::<AskPrice>::new();
        for _ in 0..3 {
            price_to_levels.insert_order(Order {
                quantity: Quantity::zero(),
                ..order()
            });
        }
        crate::assert_none!(price_to_levels.best_price());
    }

    #[test]
    fn ask_order_does_not_cross_empty_book() {
        // scenario: empty book, ask order comes in
        //
        // expected result: ask order does not cross the market (no bids to cross)
        let book = Book::new();
        let ask_order = Order {
            side: Side::Ask,
            price: Price::new(100),
            ..order()
        };
        assert!(!book.does_order_cross_market(&ask_order));
    }

    #[test]
    fn bid_order_does_not_cross_empty_book() {
        // scenario: empty book, bid order comes in
        //
        // expected result: bid order does not cross the market (no asks to cross)
        let book = Book::new();
        let bid_order = Order {
            side: Side::Bid,
            price: Price::new(100),
            ..order()
        };
        assert!(!book.does_order_cross_market(&bid_order));
    }

    #[test]
    fn ask_order_crosses_market_with_higher_bid() {
        // scenario: bid at 100 on book, ask order comes in at 90
        //
        // expected result: ask order crosses the market (ask price < bid price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let ask_order = Order {
            side: Side::Ask,
            price: Price::new(90),
            ..order()
        };
        assert!(book.does_order_cross_market(&ask_order));
    }

    #[test]
    fn ask_order_crosses_market_at_equal_price() {
        // scenario: bid at 100 on book, ask order comes in at 100
        //
        // expected result: ask order crosses the market (ask price = bid price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let ask_order = Order {
            side: Side::Ask,
            price: Price::new(100),
            ..order()
        };
        assert!(book.does_order_cross_market(&ask_order));
    }

    #[test]
    fn ask_order_does_not_cross_market_with_lower_bid() {
        // scenario: bid at 100 on book, ask order comes in at 110
        //
        // expected result: ask order should NOT cross (ask price > bid price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let ask_order = Order {
            side: Side::Ask,
            price: Price::new(110),
            ..order()
        };
        // Testing current implementation behavior
        assert!(!book.does_order_cross_market(&ask_order));
    }

    #[test]
    fn bid_order_crosses_market_with_lower_ask() {
        // scenario: ask at 100 on book, bid order comes in at 110
        //
        // expected result: bid order crosses the market (bid price > ask price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let bid_order = Order {
            side: Side::Bid,
            price: Price::new(110),
            ..order()
        };
        assert!(book.does_order_cross_market(&bid_order));
    }

    #[test]
    fn bid_order_crosses_market_at_equal_price() {
        // scenario: ask at 100 on book, bid order comes in at 100
        //
        // expected result: bid order crosses the market (bid price = ask price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let bid_order = Order {
            side: Side::Bid,
            price: Price::new(100),
            ..order()
        };
        assert!(book.does_order_cross_market(&bid_order));
    }

    #[test]
    fn bid_order_does_not_cross_market_with_higher_ask() {
        // scenario: ask at 100 on book, bid order comes in at 90
        //
        // expected result: bid order should NOT cross (bid price < ask price)
        let mut book = Book::new();
        let mut log = Log::new();
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let bid_order = Order {
            side: Side::Bid,
            price: Price::new(90),
            ..order()
        };
        // Testing current implementation behavior
        assert!(!book.does_order_cross_market(&bid_order));
    }

    #[test]
    fn ask_order_crosses_market_with_multiple_bids() {
        // scenario: multiple bids on book (90, 95, 100), ask order comes in at 92
        //
        // expected result: ask order crosses the market (crosses the best bid at 100)
        let mut book = Book::new();
        let mut log = Log::new();

        // Add multiple bids
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(90),
                ..order()
            },
            &mut log,
        )
        .unwrap();
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(95),
                ..order()
            },
            &mut log,
        )
        .unwrap();
        book.execute(
            Order {
                side: Side::Bid,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let ask_order = Order {
            side: Side::Ask,
            price: Price::new(92),
            ..order()
        };
        assert!(book.does_order_cross_market(&ask_order));
    }

    #[test]
    fn bid_order_crosses_market_with_multiple_asks() {
        // scenario: multiple asks on book (100, 105, 110), bid order comes in at 108
        //
        // expected result: bid order crosses the market (crosses the best ask at 100)
        let mut book = Book::new();
        let mut log = Log::new();

        // Add multiple asks
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(100),
                ..order()
            },
            &mut log,
        )
        .unwrap();
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(105),
                ..order()
            },
            &mut log,
        )
        .unwrap();
        book.execute(
            Order {
                side: Side::Ask,
                price: Price::new(110),
                ..order()
            },
            &mut log,
        )
        .unwrap();

        let bid_order = Order {
            side: Side::Bid,
            price: Price::new(108),
            ..order()
        };
        assert!(book.does_order_cross_market(&bid_order));
    }

    #[test]
    fn cancel_order_removes_from_book() {
        // scenario: add an order to the book, cancel it.
        //
        // expected result: the operation returns the the order was cancelled.
        // A cancellation event is found in the log. A subsequent cancel returns
        // that no order was cancelled.
        let mut book = Book::new();
        let mut log = Log::new();

        let order_id = Id::new(uuid::Uuid::new_v4());

        let order = Order {
            id: order_id,
            ..order()
        };
        book.execute(order, &mut log).unwrap();

        assert!(book.cancel(&order_id, &mut log));
        assert!(!book.contains(&order_id));

        crate::assert_some!(log.iter().find(|event| {
            event
                .as_cancel()
                .is_some_and(|cancel| cancel.id == order_id)
        }));

        assert!(
            !book.cancel(&order_id, &mut log),
            "second cancel should return false"
        );
    }

    #[test]
    fn cancel_nonexistent_order_returns_false() {
        // scenario: attempt to cancel an order that was never added to the book
        //
        // expected result: cancel returns false and no events are logged
        let mut book = Book::new();
        let mut log = Log::new();
        let nonexistent_id = Id::new(uuid::Uuid::new_v4());

        assert!(!book.cancel(&nonexistent_id, &mut log));
        assert!(log.is_empty());
    }
}
