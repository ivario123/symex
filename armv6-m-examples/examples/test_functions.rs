//! Blinks the LED on a Pico board
//!
//! This will blink an LED attached to GP25, which is the pin the Pico uses for the on-board LED.
#![no_std]
#![no_main]

// Add the bootloader
#[link_section = ".boot2"]
#[used]
pub static BOOT_LOADER: [u8; 256] = rp2040_boot2::BOOT_LOADER_W25Q080;

use defmt::*;
use defmt_rtt as _;
use panic_probe as _;
// use panic_halt as _;

use core::arch::asm;

use rp2040_boot2;
use rp2040_hal as hal;
use symex_lib::{any, assume};

use hal::{
    clocks::{init_clocks_and_plls, Clock},
    entry, pac,
    sio::Sio,
    watchdog::Watchdog,
};

#[inline(never)]
#[no_mangle]
fn panic_test_defmt(n: u8) -> u8 {
    if n < 8 {
        test_simple_if(n)
    } else {
        defmt::panic!()
    }
}

#[inline(never)]
#[no_mangle]
fn panic_test_core(n: u8) -> u8 {
    if n < 8 {
        test_simple_if(n)
    } else {
        core::panic!()
    }
}

#[inline(never)]
#[no_mangle]
fn panic_test_divide_by_zero(n: u8) -> u8 {
    if n < 8 {
        16 / (test_simple_if(n) - 1)
    } else {
        core::panic!()
    }
}

#[inline(never)]
#[no_mangle]
fn simple_loop(n: u8) -> u8 {
    let mut sum = 0;
    for i in 0..n {
        sum += test_simple_if(i);
    }
    sum
}

#[inline(never)]
#[no_mangle]
fn test_nested_if_over_function(n: u8) -> u8 {
    if n > 5 {
        99
    } else {
        test_simple_if(n)
    }
}

#[inline(never)]
#[no_mangle]
fn test_assume(n: u8) -> u8 {
    assume(n < 5);
    simple_loop(n)
}

#[inline(never)]
#[no_mangle]
fn nop_loop() {
    for _ in 0..10000 {
        unsafe {
            asm!("nop");
        }
    }
}

#[inline(never)]
#[no_mangle]
fn test_simple_if(n: u8) -> u8 {
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
fn test_simple_bool(n: bool) -> u8 {
    if n {
        1
    } else {
        5
    }
}

#[inline(never)]
#[no_mangle]
fn test_any() -> u8 {
    let n = any();
    test_simple_if(n)
}

#[inline(never)]
#[no_mangle]
fn test_any_u32() -> u32 {
    let n: u32 = any();
    if n < 4 {
        45
    } else {
        1
    }
}

#[inline(never)]
#[no_mangle]
fn test_any_u64() -> u64 {
    let n: u64 = any();
    if n < 4 {
        45
    } else {
        1
    }
}

fn run_function(f: fn(u8) -> u8) {
    for n in 0..u8::MAX {
        let r = f(n);
        info!("{} {}", n, r);
    }
}

#[entry]
fn main() -> ! {
    info!("Program start");
    let mut pac = pac::Peripherals::take().unwrap();
    let mut core = pac::CorePeripherals::take().unwrap();
    let mut watchdog = Watchdog::new(pac.WATCHDOG);
    let sio = Sio::new(pac.SIO);

    // External high-speed crystal on the pico board is 12Mhz
    let external_xtal_freq_hz = 12_000_000u32;
    let clocks = init_clocks_and_plls(
        external_xtal_freq_hz,
        pac.XOSC,
        pac.CLOCKS,
        pac.PLL_SYS,
        pac.PLL_USB,
        &mut pac.RESETS,
        &mut watchdog,
    )
    .ok()
    .unwrap();

    run_function(test_simple_if);
    run_function(test_nested_if_over_function);
    run_function(simple_loop);
    run_function(panic_test_divide_by_zero);
    run_function(panic_test_core);
    run_function(panic_test_defmt);
    run_function(test_assume);
    //run_function(test_simple_bool);
    let n = test_simple_bool(true);
    info!("n: {}", n);
    let n = test_any();
    info!("n: {}", n);
    let n = test_any_u32();
    info!("n: {}", n);
    let n = test_any_u64();
    info!("n: {}", n);
    nop_loop();
    loop {}
}

// End of file
