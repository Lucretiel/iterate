use core::fmt::Debug;
use std::iter::FusedIterator;

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

Most usefully, `iterate` can capture any other [`Iterator`] type and
iterate over it, by prefixing the value with `..`:

```
use iterate::iterate;

let range = 0..5;
let vec = vec![4, 1, 2, 3];

let iterator = iterate![
    1,
    ..range,
    ..vec.iter().copied(),
    10
];

let result: Vec<i32> = iterator.collect();
```


*/
pub use iterate_proc_macro::iterate;

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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
