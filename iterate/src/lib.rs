/*!
Powerful macro for creating iterators on the fly. See [`iterate`] for details.
*/

#![no_std]

use core::fmt;
use core::fmt::Debug;
use core::fmt::Formatter;
use core::iter::FusedIterator;

/**
Create an iterator on the fly

The `iterate` macro creates an iterator on the fly:

```
use iterate::iterate;

let mut iterator = iterate![1, 2, 3];
assert_eq!(iterator.next(), Some(1));
assert_eq!(iterator.next(), Some(2));
assert_eq!(iterator.next(), Some(3));
assert_eq!(iterator.next(), None);
```

The iterator lazily evaluates each of its arguments, one at a time:

```
use std::cell::Cell;
use iterate::iterate;

let cell = Cell::new(0);
let cell = &cell;

let mut iterator = iterate![
    {cell.set(cell.get() + 1); 1},
    {cell.set(cell.get() + 1); 2},
    {cell.set(cell.get() + 1); 3},
];

assert_eq!(cell.get(), 0);
assert_eq!(iterator.next(), Some(1));

assert_eq!(cell.get(), 1);
assert_eq!(iterator.next(), Some(2));

assert_eq!(cell.get(), 2);
assert_eq!(iterator.next(), Some(3));

assert_eq!(cell.get(), 3);
assert_eq!(iterator.next(), None);
```

Most usefully, `iterate` can capture any other [`IntoIterator`] type and
iterate over it, by prefixing the value with `..`:

```
use iterate::iterate;

let range = 0..5;
let vec = vec![4, 1, 2, 3];

// Iterate captures its arguments by move, and evaluates them lazily, so we
// need to ensure that the vec is captured by reference
let vec = &vec;

let iterator = iterate![
    1,
    ..range,
    ..vec.iter().copied(),
    10
];

let result: Vec<i32> = iterator.collect();
assert_eq!(result, [
    1,
    0, 1, 2, 3, 4,
    4, 1, 2, 3,
    10,
])
```

# Eager and Lazy evaluation

`iterate` tries to be smart about when it evaluates its arguments eagerly vs
lazily. In general it tries to evaluate them lazily, but in cases where it's
sure there will be no side effects, it evaluates `..iter` arguments eagerly.
It does this so that it can provide a more reliable `size_hint`.
*/
pub use iterate_proc_macro::iterate as iterate_raw;
#[macro_export]
macro_rules! iterate {
    ($($args:tt)*) => { $crate::iterate_raw!($crate unsafe ($($args)*)) }
}

/// The [`iterate`] macro produces a type that uses unsafe in the
/// implementation. It is therefore critical that users of the library not
/// have access to the fields of the type. Because the type is defined locally,
/// the only way to do this is to wrap it in another type that forwards the
/// iterator implementation
#[derive(Clone, Default)]
#[doc(hidden)]
pub struct ConcealedIterator<I> {
    iter: I,
}

impl<I: Debug> Debug for ConcealedIterator<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.iter.fmt(f)
    }
}

impl<I: Iterator> Iterator for ConcealedIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<I: DoubleEndedIterator> DoubleEndedIterator for ConcealedIterator<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()
    }
}

impl<I: ExactSizeIterator> ExactSizeIterator for ConcealedIterator<I> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<I: FusedIterator> FusedIterator for ConcealedIterator<I> {}

#[doc(hidden)]
#[inline]
pub fn conceal<I: Iterator>(iter: I) -> ConcealedIterator<I> {
    ConcealedIterator { iter }
}

/// Create a (0, None) size hint, representing any possible iterator
#[inline]
#[must_use]
pub const fn any_size_hint() -> (usize, Option<usize>) {
    (0, None)
}

/// Helper function for creating a size hint from a known value
#[inline]
#[must_use]
pub const fn exact_size_hint(size: usize) -> (usize, Option<usize>) {
    (size, Some(size))
}

/// Helper function for adding together a pair of size hints
#[inline]
#[must_use]
pub const fn add_size_hints(
    (lower1, upper1): (usize, Option<usize>),
    (lower2, upper2): (usize, Option<usize>),
) -> (usize, Option<usize>) {
    let lower = lower1.saturating_add(lower2);
    let upper = match (upper1, upper2) {
        (Some(upper1), Some(upper2)) => upper1.checked_add(upper2),
        _ => None,
    };
    (lower, upper)
}
