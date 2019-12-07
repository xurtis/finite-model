Finite model checking with Rust
===============================

This library provides a set of tools for describing finite model states and verifying static
and temporal properties by exhaustively searching the state space for counterexamples.

Specifying and limiting a state space
-------------------------------------

State spaces are described in terms of structures implementing `FiniteSet` which allows for
automatic generation of every value of that type within the state space. The trait can be
implemented directly or derived, such as in the following example:

```rust
#[derive(Clone, FiniteSet)]
enum State {
    Initial,
    First {
        #[finite_set(values = "vec![8, 16, 32]")]
        alpha: i32,
        #[finite_set(values = "12..8")]
        beta: u64,
        #[finite_set(lengths = "vec![3, 5, 7]")]
        delta: Vec<bool>,
    },
    Second(bool),
    Final(#[finite_set(values = "16..=24")] u8)
}
```

To see more about deriving the trait and working with value constraints and collections see the
documentation for the `FiniteSet` derive attribute.
