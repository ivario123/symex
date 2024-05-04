#![no_std]
#![no_main]
//! Simple example.
//!
//! ```shell
//! cargo symex --example get_sign --function get_sign
//! ```
use panic_halt as _;
use rp2040_hal::entry;
use symex_lib::Any;

#[inline(never)]
pub fn get_sign_inner(v: i32) -> i32 {
    if v > 0 {
        return 1;
    } else if v == 0 {
        return 0;
    } else {
        return -1;
    }
}

#[inline(never)]
#[no_mangle]
pub fn get_sign() -> i32 {
    let v = i32::any();
    get_sign_inner(v)
}

#[entry]
fn main() -> ! {
    let n = get_sign();

    unsafe {
        let _ = core::ptr::read_volatile(&n);
    }

    loop {}
}
