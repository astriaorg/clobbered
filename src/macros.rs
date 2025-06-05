#[macro_export]
macro_rules! assert_ok {
    ($cond:expr $(,)?) => {
        match $cond {
            Ok(t) => t,
            Err(e) => {
                panic!("assertion failed, expected Ok(..), got Err({e:?})");
            }
        }
    };
    ($cond:expr, $($arg:tt)+) => {
        match $cond {
            Ok(t) => t,
            Err(e) => {
                panic!("assertion failed, expected Ok(..), got Err({e:?}): {}", format_args!($($arg)+));
            }
        }
    }
}
#[macro_export]
macro_rules! assert_none {
    ($cond:expr $(,)?) => {
        match $cond {
            Some(t) => {
                panic!("assertion failed, expected None, got Some({t:?})");
            }
            None => {}
        }
    };
    ($cond:expr, $($arg:tt)+) => {
        match $cond {
            Some(t) => {
                panic!("assertion failed, expected None, Some({t:?}): {}", format_args!($($arg)+));
            }
            None => {}
        }
    }
}

#[macro_export]
macro_rules! assert_some {
    ($cond:expr $(,)?) => {
        match $cond {
            Some(t) => t,
            None => {
                panic!("assertion failed, expected Some(..), got None");
            }
        }
    };
    ($cond:expr, $($arg:tt)+) => {
        match $cond {
            Some(t) => t,
            None => {
                panic!("assertion failed, expected Some(..), got None: {}", format_args!($($arg)+));
            }
        }
    }
}

#[macro_export]
macro_rules! debug_assert_some {
    ($($arg:tt)*) => (if core::cfg!(debug_assertions) { $crate::assert_some!($($arg)*); })
}
