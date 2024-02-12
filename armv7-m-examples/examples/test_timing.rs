//! Blinks the LED on a Pico board
//!
//! This will blink an LED attached to GP25, which is the pin the Pico uses for the on-board LED.
#![no_std]
#![no_main]

// Add the bootloader
#[link_section = ".boot2"]
#[used]
pub static BOOT_LOADER: [u8; 256] = rp2040_boot2::BOOT_LOADER_W25Q080;
//pub static BOOT_LOADER: [u8; 256] = rp2040_boot2::BOOT_LOADER_RAM_MEMCPY;

use cortex_m::peripheral::{syst::SystClkSource, SYST};
use defmt::*;
use defmt_rtt as _;
use panic_probe as _;
// use panic_halt as _;

use core::arch::asm;

use rp2040_boot2;
use rp2040_hal as hal;
use symex_lib::{any, assume};
use symex_lib::{end_cyclecount, start_cyclecount};

use hal::{
    clocks::{init_clocks_and_plls, Clock},
    entry, pac,
    sio::Sio,
    watchdog::Watchdog,
};

#[inline(never)]
#[no_mangle]
fn measure_symex() {
    start_cyclecount();
    nop_loop();
    end_cyclecount();
}

#[inline(never)]
#[no_mangle]
//#[link_section = ".ram_code"]
fn nop_loop() {
    for _ in 0..10000 {
        unsafe {
            asm!("nop");
        }
    }
}

#[no_mangle]
#[inline(never)]
//#[link_section = ".ram_code"]
fn measure_hw() -> u32 {
    let start = SYST::get_current();
    nop_loop();
    let stop = SYST::get_current();
    start - stop
}

#[entry]
fn main() -> ! {
    info!("Program start");
    let mut pac = pac::Peripherals::take().unwrap();

    // try disable cache
    //pac.XIP_CTRL.ctrl.write(|w| {w.en().bit(false)});

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

    measure_symex();

    let systic_reload_time: u32 = 0x00ffffff;
    let mut systic = core.SYST;
    systic.set_clock_source(SystClkSource::Core);
    systic.set_reload(systic_reload_time);
    systic.enable_counter();
    let cycles = measure_hw();
    info!("cycles: {}", cycles);
    info!("program end");
    loop {}
}

// End of file
// 40065 measured rp2040
// 40016 counted symex
