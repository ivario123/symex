#![no_std]
#![no_main]
//! Simple example showcasing what symbolic execution can do.
//!
//! ```shell
//! cargo symex --example initial --function foo
//! ```
#![allow(dead_code)]
use panic_halt as _;
use symex_lib::Any;

#[inline(never)]
#[no_mangle]
fn bar(x: i32, y: i32) -> i32 {
    cortex_m::asm::nop();
    if x > 5 && x + y == 100 {
        if x * y == 1875 {
            panic!();
        } else {
            5
        }
    } else {
        x / y
    }
}

#[inline(never)]
#[no_mangle]
fn foo() -> i32 {
    cortex_m::asm::nop();
    let x = i32::any();
    let y = i32::any();
    bar(x, y)
}

#[cortex_m_rt::entry]
fn main() -> ! {
    let n = foo();

    unsafe {
        let _ = core::ptr::read_volatile(&n);
    }

    loop {}
}
