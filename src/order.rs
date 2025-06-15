#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NoPrice,
    NoSide,
    NoStopPrice,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Id(uuid::Uuid);

impl Id {
    pub fn new(uuid: uuid::Uuid) -> Self {
        Self(uuid)
    }
}

/// A non-negative price of an asset.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Price(u128);

impl Price {
    /// Constructs a new price.
    pub const fn new(value: u128) -> Self {
        Self(value)
    }

    /// Returns the maximum possible price.
    ///
    /// This is useful when used in lieu of the best possible bid/worst possible ask price.
    pub const fn max() -> Self {
        Self::new(u128::MAX)
    }

    /// Returns the lowest possible price/zero.
    ///
    /// This is useful when used in lieu of the worst possible bid/best possible ask price.
    pub const fn zero() -> Self {
        Self::new(0)
    }

    /// Calculates the price plus slippage.
    ///
    /// This method saturates at the maximum of the underlying primitive.
    pub fn plus_slippage(&self, slippage: &Slippage) -> Self {
        Self(self.0.saturating_add(slippage.0))
    }

    /// Calculates the price minus slippage.
    ///
    /// This method saturates at the minimum of the underlying primitive.
    ///
    /// # Examples
    ///
    /// ```
    /// # use clobbered::order::{Price, Slippage};
    /// let price = Price::new(10);
    /// assert_eq!(price.minus_slippage(&Slippage::new(5)), Price::new(5));
    /// assert_eq!(price.minus_slippage(&Slippage::new(2)), Price::new(8));
    /// ```
    pub fn minus_slippage(&self, slippage: &Slippage) -> Self {
        Self(self.0.saturating_sub(slippage.0))
    }

    /// Returns if the price is better than or equal to `other` depending on `side`.
    ///
    /// If `side` is `Ask`, then this method returns `true` if `self <= other`.
    /// If `side` is `Bid`, then this method return `true` if `self >= other`.
    ///
    /// # Examples
    /// ```
    /// # use clobbered::order::{Price, Side};
    /// let price = Price::new(20);
    /// assert!(price.is_better_than_or_equal_to(&Price::new(30), &Side::Ask));
    /// assert!(!price.is_better_than_or_equal_to(&Price::new(30), &Side::Bid));
    /// ```
    pub fn is_better_than_or_equal_to(&self, other: &Self, side: &Side) -> bool {
        match side {
            Side::Ask => self <= other,
            Side::Bid => self >= other,
        }
    }
}

impl AsRef<Price> for Price {
    fn as_ref(&self) -> &Price {
        self
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slippage(u128);

impl Slippage {
    pub fn new(value: u128) -> Self {
        Self(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Quantity(u128);

impl Quantity {
    pub(crate) fn new(value: u128) -> Self {
        Self(value)
    }

    pub(crate) fn get(self) -> u128 {
        self.0
    }

    pub(crate) fn inner(&self) -> &u128 {
        &self.0
    }

    pub fn is_zero(&self) -> bool {
        self.0 == u128::MIN
    }

    pub fn zero() -> Self {
        Self(u128::MIN)
    }

    pub fn saturating_add(&self, other: &Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    pub fn saturating_sub(&self, other: &Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }
}
pub struct Timestamp(std::time::Instant);

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Ask = 0,
    Bid = 1,
}

impl Side {
    pub fn is_ask(&self) -> bool {
        self == &Self::Ask
    }

    pub fn is_bid(&self) -> bool {
        self == &Self::Bid
    }

    /// Returns the opposite of `self.`
    ///
    /// # Examples
    ///
    /// ```
    /// # use clobbered::order::Side;
    /// let ask = Side::Ask;
    /// let bid = Side::Bid;
    /// assert_eq!(ask.opposite(), bid);
    /// assert_eq!(bid.opposite(), ask);
    /// assert_eq!(ask.opposite().opposite(), ask);
    /// ```
    pub fn opposite(&self) -> Side {
        match self {
            Side::Ask => Side::Bid,
            Side::Bid => Side::Ask,
        }
    }
}

/// The type of the order.
///
/// Explanations:
///
/// + `Market` orders are executed at the best available price.
/// + `Limit` orders are executed at the provided price or better (
///   lower prices for bids, higher for asks).
/// + `Stop` orders are executed once a stop price is reached, at which
///   point it becomes a market order. A stop buy order is entered at
///   at a stop price above the current market price. A sell stop order
///   is entered below the current market price.
/// + `StopLimit` orders combine `Stop` orders with `Limit` orders: once
///   the stop price is reached the order is converted into a limit order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Type {
    Limit,
    Market,
    Stop,
    StopLimit,
    // TrailingStop,
    // TrailingStopLimit,
}

impl Type {
    fn is_stop(&self) -> bool {
        matches!(self, Self::Stop | Self::StopLimit)
    }
}

/// + `GoodTillCanceled`: the order stays live until it's fulfilled or cancelled.
/// + `ImmediateOrCancel`: the order is executed immediately. Any portion of the
///   order that cannot be fulfilled is cancelled (partial execution is allowed).
/// + `FillOrKill`: the order must be executed immediately and in its entirety. It
///   is cancelled otherwise (no partial execution is allowed).
/// + `AllOrNone`: the order must be executed in its entirety. If it cannot be
///   executed in its entirety it will remain active until executed or cancelled.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TimeInForce {
    GoodTillCanceled,
    ImmediateOrCancel,
    FillOrKill,
    AllOrNone,
    // GoodTillDate,
}

pub struct Builder {
    id: Id,
    type_: Type,
    time_in_force: TimeInForce,
    side: Option<Side>,
    quantity: Quantity,
    price: Option<Price>,
    stop_price: Option<Price>,
    slippage: Option<Slippage>,
    post_only: bool,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            id: Id::new(uuid::Uuid::nil()),
            type_: Type::Limit,
            time_in_force: TimeInForce::GoodTillCanceled,
            side: None,
            quantity: Quantity::zero(),
            price: None,
            stop_price: None,
            slippage: None,
            post_only: false,
        }
    }

    pub fn id(self, id: Id) -> Self {
        Self { id, ..self }
    }

    pub fn type_(self, type_: Type) -> Self {
        Self { type_, ..self }
    }

    pub fn time_in_force(self, time_in_force: TimeInForce) -> Self {
        Self {
            time_in_force,
            ..self
        }
    }

    pub fn side(self, side: Side) -> Self {
        Self {
            side: Some(side),
            ..self
        }
    }

    pub fn quantity(self, quantity: Quantity) -> Self {
        Self { quantity, ..self }
    }

    pub fn price(self, price: Price) -> Self {
        Self {
            price: Some(price),
            ..self
        }
    }

    pub fn stop_price(self, stop_price: Price) -> Self {
        Self {
            stop_price: Some(stop_price),
            ..self
        }
    }

    pub fn slippage(self, slippage: Slippage) -> Self {
        Self {
            slippage: Some(slippage),
            ..self
        }
    }

    pub fn post_only(self, post_only: bool) -> Self {
        Self { post_only, ..self }
    }

    pub fn build(self) -> Result<Order, Error> {
        let Self {
            id,
            type_,
            time_in_force,
            side,
            quantity,
            price,
            stop_price,
            slippage,
            post_only,
        } = self;
        let side = side.ok_or(Error::NoSide)?;
        let price = price.ok_or(Error::NoPrice)?;

        // XXX: For stop orders The stop price must be set. Otherwise the value itself
        // doesn't matter and is just passed through (the orderbook will not look at it).
        let stop_price = stop_price
            .or_else(|| {
                if type_.is_stop() {
                    None
                } else {
                    Some(Price::zero())
                }
            })
            .ok_or(Error::NoStopPrice)?;

        Ok(Order {
            id,
            type_,
            time_in_force,
            side,
            quantity,
            price,
            stop_price,
            slippage,
            post_only,
        })
    }
}

/// An order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Order {
    /// The ID assigned to the order upon entering the system.
    pub(crate) id: Id,
    // The type of order (market, limit, etc)
    //
    // NOTE: type is a reserved keyword in rust, hence we call this field type_.
    pub(crate) type_: Type,
    // The time in force of the order (good-till-cancelled, fill-or-kill, etc)
    pub(crate) time_in_force: TimeInForce,
    // side - buy or sell
    pub(crate) side: Side,
    // the units to buy
    pub(crate) quantity: Quantity,
    // the price in ticks supported by the orderbook
    pub(crate) price: Price,
    // the stop price of the order
    pub(crate) stop_price: Price,
    // the permitted slippage of the order.
    //
    // Slippage is only considered for market orders and allows limiting the price range to
    // [<market_price>, <best_price> + <slippage>] (for bids), and
    // [<market_price> - <slippage>, <market_price>] (for asks), respectively.
    //
    // If slippage is not set then slippage is considered to be infinite: the market order
    // will be filled no matter the price (this is the most dangerous) setting.
    //
    // If slippage is set to 0, the market order will only be executed at exactly the market price.
    // The unfilled portion of the order is then cancelled.
    pub(crate) slippage: Option<Slippage>,
    // If the order is intended to be posted to the orderbook only without execution.
    // This is useful for markets makers who don't want to (partially) execute an order and only
    // have the order added to the book if it wouldn't cross the market.
    pub(crate) post_only: bool,
    // // the time that the order entered the book
    // pub(crate) entry_time: Timestamp,
}

impl Order {
    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn price(&self) -> &Price {
        &self.price
    }

    pub fn quantity(&self) -> &Quantity {
        &self.quantity
    }

    pub fn side(&self) -> &Side {
        &self.side
    }

    pub fn slippage(&self) -> Option<&Slippage> {
        self.slippage.as_ref()
    }

    pub fn stop_price(&self) -> &Price {
        &self.stop_price
    }

    /// Returns the type of the order.
    ///
    /// NOTE: `type` is a reserved keyword in Rust, hence this is called `type_`.
    pub fn type_(&self) -> &Type {
        &self.type_
    }

    /// Returns if the order is on the ask-side.
    pub fn is_ask(&self) -> bool {
        self.side.is_ask()
    }

    /// Returns if the order is on the bid-side.
    pub fn is_bid(&self) -> bool {
        self.side.is_bid()
    }

    /// Returns if the order is an executable order.
    ///
    /// Executable orders are all limit and market orders (i.e.
    /// not any of flavor of stop order).
    pub fn is_executable(&self) -> bool {
        matches!(self.type_, Type::Market | Type::Limit)
    }

    pub fn is_fill_or_kill(&self) -> bool {
        matches!(self.time_in_force, TimeInForce::FillOrKill)
    }

    /// Returns if the order is filled, i.e. if its quantity is zero.
    pub fn is_filled(&self) -> bool {
        self.quantity.is_zero()
    }

    /// Returns if an order requires immediate execution.
    ///
    /// This applies to all fill-or-kill, immediate-or-cancel,
    /// and market orders.
    pub fn is_immediate(&self) -> bool {
        matches!(self.type_, Type::Market,)
            || matches!(
                self.time_in_force,
                TimeInForce::FillOrKill | TimeInForce::ImmediateOrCancel
            )
    }

    /// Returns if the order is a market order.
    pub fn is_market(&self) -> bool {
        matches!(self.type_, Type::Market,)
    }

    /// Returns if the order is post-only.
    ///
    /// Note that in order for orders to be post-only they must
    /// have their post-only flag and also not be market orders
    /// and not be fill-or-kill or immediate-or-cancel.
    pub fn is_post_only(&self) -> bool {
        self.post_only && !self.is_market() && !self.is_immediate()
    }

    /// Returns if the order is a stop order.
    pub fn is_stop(&self) -> bool {
        self.type_.is_stop()
    }

    /// Sets the order price to [`price`].
    pub fn set_price(&mut self, price: Price) {
        self.price = price;
    }

    /// Returns if the order has slippage set.
    pub fn has_slippage(&self) -> bool {
        self.slippage.is_some()
    }

    /// Returns if the order has volume, that is if its quantity is not zero.
    ///
    /// This is the inverse of [`Order::is_filled`].
    pub fn has_volume(&self) -> bool {
        self.quantity != Quantity::zero()
    }

    pub fn needs_full_execution(&self) -> bool {
        matches!(
            self.time_in_force,
            TimeInForce::FillOrKill | TimeInForce::AllOrNone
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, Order, Price, Side, Type};

    #[test]
    fn orders_must_have_a_side_set() {
        assert_eq!(
            Err(Error::NoSide),
            Order::builder().price(Price::new(5)).build(),
        );
    }

    #[test]
    fn orders_must_have_a_price_set() {
        assert_eq!(
            Err(Error::NoPrice),
            Order::builder().side(Side::Ask).build(),
        );
    }

    #[test]
    fn stop_orders_must_have_stop_price_set() {
        assert_eq!(
            Err(Error::NoStopPrice),
            Order::builder()
                .type_(Type::Stop)
                .price(Price::new(5))
                .side(Side::Ask)
                .build(),
        );
    }
}
