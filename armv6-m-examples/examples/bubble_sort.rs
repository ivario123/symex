#![no_std]
#![no_main]
//! Bubble-sort example.
//!
//! This example is a degenerate case which is hard to analyze, so expect running times to be high.
//!
//! ```shell
//! cargo symex --example bubble_sort
//! ```
#![allow(dead_code)]
use panic_halt as _;
use rp2040_hal::entry;
use symex_lib::symbolic;

fn bubble_sort(mut vec: [i32; 3]) -> [i32; 3] {
    loop {
        let mut done = true;
        for i in 0..vec.len() - 1 {
            if vec[i + 1] < vec[i] {
                done = false;
                let temp = vec[i + 1];
                vec[i + 1] = vec[i];
                vec[i] = temp;
            }
        }
        if done {
            return vec;
        }
    }
}

#[inline(never)]
#[no_mangle]
fn test() {
    let mut a = [0; 3];
    symbolic(&mut a);
    bubble_sort(a);
}

#[entry]
fn main() -> ! {
    test();

    loop {}
}
