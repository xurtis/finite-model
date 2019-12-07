//! Finite model checking
//!
//! This library provides a set of tools for describing finite model states and verifying static
//! and temporal properties by exhaustively searching the state space for counterexamples.
//!
//! Specifying and limiting a state space
//! =====================================
//!
//! State spaces are described in terms of structures implementing
//! [`FiniteSet`](trait.FiniteSet.html) which allows for automatic generation of every value of
//! that type within the state space. The trait can be implemented directly or derived, such as in
//! the following example:
//!
//! ```rust
//! #[derive(Clone, FiniteSet)]
//! enum State {
//!     Initial,
//!     First {
//!         #[finite_set(values = "vec![8, 16, 32]")]
//!         alpha: i32,
//!         #[finite_set(values = "12..8")]
//!         beta: u64,
//!         #[finite_set(lengths = "vec![3, 5, 7]")]
//!         delta: Vec<bool>,
//!     },
//!     Second(bool),
//!     Final(#[finite_set(values = "16..=24")] u8)
//! }
//! ```
//!
//! To see more about deriving the trait and working with value constraints and collections see the
//! documentation for the [`FiniteSet`](derive.FiniteSet.html) derive attribute.

pub use finite_model_derive::FiniteSet;

mod finite_set_impl;

/// Implementations for the `FiniteSet` trait for language primitive types.
pub mod r#impl {
    pub use crate::finite_set_impl::*;
}

pub use finite_set_impl::{EmptyIter, UnitIter};

/// A trait that produces every possible value of the given type
pub trait FiniteSet: Sized + Clone {
    type Iter: Iterator<Item = Self>;

    /// Produce an iterator over all values of a given type within the state space
    fn finite_set() -> Self::Iter;

    /// Choose an arbitrary element
    fn choose() -> Option<Self> {
        Self::finite_set().next()
    }

    /// Choose an arbitrary element satisfying the constraint
    fn satisfying(predicate: impl Fn(&Self) -> bool) -> Option<Self> {
        Self::finite_set().filter(predicate).next()
    }
}

/// A trait on collections for creating a sets of collections with limited lengths
pub trait CollectionSet<I: Iterator<Item = usize>>: Sized + Clone {
    type Iter: Iterator<Item = Self>;

    /// Iterate over all of the elements in the set
    fn finite_set(lengths: I) -> Self::Iter;

    /// Choose an arbitrary element
    fn choose(lengths: I) -> Option<Self> {
        Self::finite_set(lengths).next()
    }

    /// Choose an arbitrary element satisfying the constraint
    fn satisfying(lengths: I, predicate: impl Fn(&Self) -> bool) -> Option<Self> {
        Self::finite_set(lengths).filter(predicate).next()
    }
}

/// Combinator to join sets of multiple fields together
pub struct Multiplier<I, H: FiniteSet>(H::Iter, I);

impl<T, H: FiniteSet> Multiplier<T, H> {
    pub fn new(tail: T) -> Self {
        Multiplier(H::finite_set(), tail)
    }
}

impl<T, H> Iterator for Multiplier<T, H>
where
    H: FiniteSet,
    T: Clone,
{
    type Item = (T, H);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|h| (self.1.clone(), h))
    }
}

/// Combinator for generating different sizes of collections
pub struct CollectionMultiplier<I: Iterator<Item = usize>, T: Clone, H: CollectionSet<I>>(H::Iter, T);

impl<I: Iterator<Item = usize>, T: Clone, H: CollectionSet<I>> CollectionMultiplier<I, T, H> {
    pub fn new(length: I, tail: T) -> Self {
        CollectionMultiplier(H::finite_set(length), tail)
    }
}

impl<I, T, H> Iterator for CollectionMultiplier<I, T, H>
where
    I: Iterator<Item = usize>,
    H: CollectionSet<I>,
    T: Clone,
{
    type Item = (T, H);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|h| (self.1.clone(), h))
    }
}

/// Combinator generate sets of values within a bounded range
pub struct ValuesMultiplier<I: Iterator<Item = H>, T, H>(I, T);

impl<I: Iterator<Item = H>, T, H> ValuesMultiplier<I, T, H> {
    pub fn new(range: I, tail: T) -> Self {
        ValuesMultiplier(range, tail)
    }
}

impl<I, T, H> Iterator for ValuesMultiplier<I, T, H>
where
    I: Iterator<Item = H>,
    T: Clone,
{
    type Item = (T, H);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|h| (self.1.clone(), h))
    }
}
