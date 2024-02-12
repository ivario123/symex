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
use symex_lib::{assume, symbolic, valid, Valid, Validate};

use dut::E;

#[inline(never)]
#[no_mangle]
pub fn check() -> u16 {
    let mut u: u16 = 0;
    symbolic(&mut u);

    // constructor like approach
    let e = match u {
        0 => E::A,
        1 => E::B(1),
        2 => E::C(2),
        4 => E::D(3),
        5 => E::E(4),
        _ => E::F(u),
    };

    dut::some_function(&e)
}

#[inline(never)]
#[no_mangle]
pub fn check2() -> u16 {
    let mut e = E::A;
    symbolic(&mut e);

    // defined from valid, not working in release
    valid(&e);

    dut::some_function(&e)
}

#[inline(never)]
#[no_mangle]
pub fn check3() -> u16 {
    let mut e = E::A;
    symbolic(&mut e);

    // unfortunately, still does not work in release
    assume(e.is_valid());

    dut::some_function(&e)
}

#[entry]
fn main() -> ! {
    let n0 = check();
    let n1 = check2();
    let n2 = check3();
    unsafe {
        let _ = core::ptr::read_volatile(&n0);
        let _ = core::ptr::read_volatile(&n1);
        let _ = core::ptr::read_volatile(&n2);
    }

    loop {}
}
