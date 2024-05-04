#![no_std]
#![no_main]
//! Examples for how enums are handled.
//!
//! If `symbolic` is just called on an enum `check` shows what happens.
//!
//! ```shell
//! cargo symex --example enum --function check
//! ```
//!
//! This will trigger an `UnreachableInstruction` error, as the enum should be exhaustive and
//! `symbolic` will allow for invalid variants.
//!
//! However `check_valid` shows how to handle these cases.
//!
//! ```shell
//! cargo symex --example enum --function check_valid
//! ```
//!
//! After marking the enum as symbolic,
//! the helper function `is_valid` when derived will suppress the invalid variant and only allow
//! the valid variants.
#![allow(dead_code)]
use panic_halt as _;
use rp2040_hal::entry;
use symex_lib::{symbolic, Any};

enum E {
    One,
    Two,
    Three,
    Four,
    Five,
}

impl Any for E {
    fn any() -> Self {
        let n = u8::any();
        match n {
            0 => E::One,
            1 => E::Two,
            2 => E::Three,
            3 => E::Four,
            _ => E::Five,
        }
    }
}

#[inline(never)]
#[no_mangle]
fn simple_if(n: u16) -> u16 {
    if n == 3 {
        1
    } else if n == 6 {
        5
    } else {
        2
    }
}

#[inline(never)]
#[no_mangle]
fn check() -> u16 {
    let input = E::any();
    handle_test_enum(input)
}

#[inline(never)]
#[no_mangle]
fn handle_test_enum(n: E) -> u16 {
    match n {
        E::One => 1,
        E::Two => 3,
        E::Three => 9,
        E::Four => 5,
        E::Five => 2,
    }
}

#[entry]
fn main() -> ! {
    let n0 = check();
    unsafe {
        let _ = core::ptr::read_volatile(&n0);
    }

    loop {}
}
