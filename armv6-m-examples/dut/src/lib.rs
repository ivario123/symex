#![no_std]

use symex_lib::{symbolic, valid, Validate};

#[derive(Validate)]
pub enum E {
    A,
    B(u8),
    C(u16),
    D(u16),
    E(u16),
    F(u16),
}

pub fn some_function(e: &E) -> u16 {
    match e {
        E::A => 1,
        E::B(_u) => 2,
        E::C(_u) => 3,
        E::D(_u) => 4,
        E::E(_u) => 5,
        E::F(_u) => 6,
    }
}
