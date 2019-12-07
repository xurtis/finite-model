//! Implementation of `FiniteSet` for basic types

use crate::{FiniteSet, CollectionSet};
use std::collections::{VecDeque, LinkedList, HashMap, BTreeMap, HashSet, BTreeSet, BinaryHeap};
use std::iter::FromIterator;

macro_rules! numeric_impls {
    [$($ty:ident),*$(,)?] => {
        $(
            impl FiniteSet for $ty {
                type Iter = std::ops::Range<$ty>;

                fn finite_set() -> Self::Iter {
                    std::$ty::MIN..std::$ty::MAX
                }
            }
        )*
    };
}

numeric_impls![u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize];

/// An iterator over option values
pub struct OptionIter<I: FiniteSet>(Option<I::Iter>);

impl<I: FiniteSet> Default for OptionIter<I> {
    fn default() -> Self {
        OptionIter(Some(I::finite_set()))
    }
}

impl<I: FiniteSet> Iterator for OptionIter<I> {
    type Item = Option<I>;

    fn next(&mut self) -> Option<Self::Item> {
        let (curr, next) = match self.0.take() {
            Some(mut iter) => {
                match iter.next() {
                    Some(next) => (Some(Some(next)), Some(iter)),
                    None => (Some(None), None),
                }
            }
            None => (None, None),
        };

        self.0 = next;
        curr
    }
}

impl<I: FiniteSet> FiniteSet for Option<I> {
    type Iter = OptionIter<I>;

    fn finite_set() -> Self::Iter {
        OptionIter::default()
    }
}

/// An iterator over the true and false values
pub struct BoolIter(Option<bool>);

impl Default for BoolIter {
    fn default() -> Self {
        BoolIter(Some(false))
    }
}

impl Iterator for BoolIter {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        let (curr, next) = match self.0.take() {
            Some(false) => (Some(false), Some(true)),
            Some(true) => (Some(true), None),
            None => (None, None),
        };
        self.0 = next;
        curr
    }
}

impl FiniteSet for bool {
    type Iter = BoolIter;

    fn finite_set() -> Self::Iter {
        BoolIter::default()
    }
}

/// An iterator over a single unit value
pub struct UnitIter(Option<()>);

impl Default for UnitIter {
    fn default() -> Self {
        UnitIter(Some(()))
    }
}

impl Iterator for UnitIter {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        self.0.take()
    }
}

impl FiniteSet for () {
    type Iter = UnitIter;

    fn finite_set() -> Self::Iter {
        UnitIter::default()
    }
}

/// An iterator over no elements of a given type
pub struct EmptyIter<T>(::std::marker::PhantomData<T>);

impl<T> Default for EmptyIter<T> {
    fn default() -> Self {
        EmptyIter(::std::marker::PhantomData)
    }
}

impl<T> Iterator for EmptyIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

/// An iterator over arrays of a given type
pub struct PairIter<A: FiniteSet, B: FiniteSet> {
    tail: Option<A>,
    tail_iter: A::Iter,
    head_iter: B::Iter,
}

impl<A: FiniteSet, B: FiniteSet> Default for PairIter<A, B> {
    fn default() -> Self {
        let mut tail_iter = A::finite_set();
        let head_iter = B::finite_set();
        let tail = tail_iter.next();

        PairIter { tail, tail_iter, head_iter }
    }
}

impl<A: FiniteSet, B: FiniteSet> Iterator for PairIter<A, B> {
    type Item = (A, B);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match (self.tail.take(), self.head_iter.next()) {
                (None, _) => break None,
                (Some(tail), Some(head)) => {
                    self.tail = Some(tail.clone());
                    break Some((tail, head));
                }
                (Some(_), None) => {
                    self.tail = self.tail_iter.next();
                    self.head_iter = B::finite_set();
                }
            };
        }
    }
}

macro_rules! tuple_impls {
    ($struct:ident => $first:ident $(, $rstruct:ident => $rest:ident )*$(,)?) => {
        tuple_impls!($($rstruct => $rest),*);

        /// An iterator over a fixed-length tuple
        pub struct $struct<$first: FiniteSet $(, $rest: FiniteSet)*>(
            PairIter<$first, ($($rest,)*)>
        );

        impl<$first: FiniteSet $(,$rest: FiniteSet)*> FiniteSet for ($first $(,$rest)*,) {
            type Iter = $struct<$first $(, $rest)*>;

            fn finite_set() -> Self::Iter {
                unimplemented!()
            }
        }

        impl<$first: FiniteSet $(,$rest: FiniteSet)*> Iterator for $struct<$first $(,$rest)*> {
            type Item = ($first $(,$rest)*,);

            #[allow(non_snake_case)]
            fn next(&mut self) -> Option<Self::Item> {
                if let Some(($first, ($($rest,)*))) = self.0.next() {
                    Some(($first $(,$rest)*,))
                } else {
                    None
                }
            }
        }
    };
    () => {
    };
}

tuple_impls!(
    TupleIterK => K,
    TupleIterJ => J,
    TupleIterI => I,
    TupleIterH => H,
    TupleIterG => G,
    TupleIterF => F,
    TupleIterE => E,
    TupleIterD => D,
    TupleIterC => C,
    TupleIterB => B,
    TupleIterA => A,
);

/// Iterators over vectors of varying lengths
pub struct VecIter<I, T, S> {
    lengths: I,
    iters: Option<(Vec<S>, Vec<T>)>,
}

impl<I: Iterator<Item = usize>, T: FiniteSet> VecIter<I, T, T::Iter> {
    fn next_length(lengths: &mut I) -> Option<(Vec<T::Iter>, Vec<T>)> {
        let length = lengths.next();
        if let Some(length) = length {
            let mut iters = (0..length).into_iter().map(|_| T::finite_set()).collect::<Vec<_>>();
            let mut next = vec![];
            for value in iters.iter_mut().map(|i| i.next()) {
                if let Some(value) = value {
                    next.push(value);
                } else {
                    next = vec![];
                    break;
                }
            }
            Some((iters, next))
        } else {
            None
        }
    }
}

impl<I: Iterator<Item = usize>, T: FiniteSet> VecIter<I, T, T::Iter> {
    fn new(mut lengths: I) -> Self {
        let iters = VecIter::next_length(&mut lengths);
        VecIter { lengths, iters }
    }
}

impl<I: Iterator<Item = usize>, T: FiniteSet> Iterator for VecIter<I, T, T::Iter> {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((mut iters, mut next)) = self.iters.take() {
            let curr = next.clone();
            for i in 0..=iters.len() {
                if i == iters.len() {
                    self.iters = VecIter::next_length(&mut self.lengths);
                } else if let Some(value) = iters[i].next() {
                    next[i] = value;
                    self.iters = Some((iters, next));
                    break;
                } else {
                    iters[i] = T::finite_set();
                    if let Some(value) = iters[i].next() {
                        next[i] = value;
                    } else {
                        self.iters = None;
                        break;
                    }
                }
            }
            Some(curr)
        } else {
            None
        }
    }
}

impl<I: Iterator<Item = usize>, T: FiniteSet> CollectionSet<I> for Vec<T> {
    type Iter = VecIter<I, T, T::Iter>;

    fn finite_set(length: I) -> Self::Iter {
        VecIter::new(length)
    }
}

/// An iterator that iterates over collections that implement FromIterator
pub struct FromIteratorIter<I, T, F>(VecIter<I, T, T::Iter>, ::std::marker::PhantomData<F>)
where
    I: Iterator<Item = usize>,
    T: FiniteSet,
    F: FromIterator<T>;

impl <I, T, F> Iterator for FromIteratorIter<I, T, F>
where
    I: Iterator<Item = usize>,
    T: FiniteSet,
    F: FromIterator<T>
{
    type Item = F;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| v.into_iter().collect())
    }
}

macro_rules! collection_impls {
    [$($collection:ident<$($generic:ident$(: $($trait:path)*)?),*>),*$(,)?] => {$(
        #[allow(unused_parens)]
        impl<I, $($generic),*> CollectionSet<I> for $collection<$($generic),*>
        where
            I: Iterator<Item = usize>,
            $($generic: FiniteSet $($(+ $trait)*)?),*
        {
            type Iter = FromIteratorIter<I, ($($generic),*), $collection<$($generic),*>>;

            fn finite_set(length: I) -> Self::Iter {
                FromIteratorIter(VecIter::new(length), ::std::marker::PhantomData)
            }
        }
    )*};
}

collection_impls![
    VecDeque<T>,
    LinkedList<T>,
    HashMap<K: ::std::hash::Hash Eq, V>,
    BTreeMap<K: Ord, V>,
    HashSet<T: ::std::hash::Hash Eq>,
    BTreeSet<T: Ord>,
    BinaryHeap<T: Ord>,
];
