#![feature(prelude_import)]
//! Blinks the LED on a Pico board
#![no_std]
#![no_main]
#[prelude_import]
use core::prelude::rust_2021::*;
#[macro_use]
extern crate core;
extern crate compiler_builtins as _;
use core::arch::asm;
use bsp::entry;
use cortex_m::peripheral::{syst::SystClkSource, SYST};
use defmt::*;
use defmt_rtt as _;
use embedded_hal::digital::v2::OutputPin;
use panic_probe as _;
use rp_pico as bsp;
use bsp::hal::{
    clocks::{init_clocks_and_plls, Clock},
    pac, sio::Sio, watchdog::Watchdog,
};
use symex_lib::{
    end_cyclecount, any, start_cyclecount, symbolic, assume, Any, black_box,
    suppress_path, Valid, Validate,
};
#[doc(hidden)]
#[export_name = "main"]
pub unsafe extern "C" fn __cortex_m_rt_main_trampoline() {
    __cortex_m_rt_main()
}
fn __cortex_m_rt_main() -> ! {
    unsafe {
        const SIO_BASE: u32 = 0xd0000000;
        const SPINLOCK0_PTR: *mut u32 = (SIO_BASE + 0x100) as *mut u32;
        const SPINLOCK_COUNT: usize = 32;
        for i in 0..SPINLOCK_COUNT {
            SPINLOCK0_PTR.wrapping_add(i).write_volatile(1);
        }
    }
    match () {
        () => {
            if {
                const CHECK: bool = {
                    const fn check() -> bool {
                        let module_path = "enum_match_derive".as_bytes();
                        if if 17usize > module_path.len() {
                            false
                        } else {
                            module_path[0usize] == 101u8 && module_path[1usize] == 110u8
                                && module_path[2usize] == 117u8
                                && module_path[3usize] == 109u8
                                && module_path[4usize] == 95u8
                                && module_path[5usize] == 109u8
                                && module_path[6usize] == 97u8
                                && module_path[7usize] == 116u8
                                && module_path[8usize] == 99u8
                                && module_path[9usize] == 104u8
                                && module_path[10usize] == 95u8
                                && module_path[11usize] == 100u8
                                && module_path[12usize] == 101u8
                                && module_path[13usize] == 114u8
                                && module_path[14usize] == 105u8
                                && module_path[15usize] == 118u8
                                && module_path[16usize] == 101u8
                                && if 17usize == module_path.len() {
                                    true
                                } else {
                                    module_path[17usize] == b':'
                                }
                        } {
                            return true;
                        }
                        false
                    }
                    check()
                };
                CHECK
            } {
                unsafe { defmt::export::acquire() };
                defmt::export::header(
                    &{
                        defmt::export::make_istr({
                            #[link_section = ".defmt.{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"Ex1 start\",\"disambiguator\":\"12378557682773086049\",\"crate_name\":\"enum_match_derive\"}"]
                            #[export_name = "{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"Ex1 start\",\"disambiguator\":\"12378557682773086049\",\"crate_name\":\"enum_match_derive\"}"]
                            static DEFMT_LOG_STATEMENT: u8 = 0;
                            &DEFMT_LOG_STATEMENT as *const u8 as u16
                        })
                    },
                );
                unsafe { defmt::export::release() }
            }
        }
    };
    let mut pac = pac::Peripherals::take().unwrap();
    let core = pac::CorePeripherals::take().unwrap();
    let mut watchdog = Watchdog::new(pac.WATCHDOG);
    let sio = Sio::new(pac.SIO);
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
    let systic_reload_time: u32 = 0x00ffffff;
    let mut systic = core.SYST;
    systic.set_clock_source(SystClkSource::Core);
    systic.set_reload(systic_reload_time);
    systic.enable_counter();
    let r = test_any();
    match (&(r)) {
        (arg0) => {
            if {
                const CHECK: bool = {
                    const fn check() -> bool {
                        let module_path = "enum_match_derive".as_bytes();
                        if if 17usize > module_path.len() {
                            false
                        } else {
                            module_path[0usize] == 101u8 && module_path[1usize] == 110u8
                                && module_path[2usize] == 117u8
                                && module_path[3usize] == 109u8
                                && module_path[4usize] == 95u8
                                && module_path[5usize] == 109u8
                                && module_path[6usize] == 97u8
                                && module_path[7usize] == 116u8
                                && module_path[8usize] == 99u8
                                && module_path[9usize] == 104u8
                                && module_path[10usize] == 95u8
                                && module_path[11usize] == 100u8
                                && module_path[12usize] == 101u8
                                && module_path[13usize] == 114u8
                                && module_path[14usize] == 105u8
                                && module_path[15usize] == 118u8
                                && module_path[16usize] == 101u8
                                && if 17usize == module_path.len() {
                                    true
                                } else {
                                    module_path[17usize] == b':'
                                }
                        } {
                            return true;
                        }
                        false
                    }
                    check()
                };
                CHECK
            } {
                unsafe { defmt::export::acquire() };
                defmt::export::header(
                    &{
                        defmt::export::make_istr({
                            #[link_section = ".defmt.{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"r: {}\",\"disambiguator\":\"8069176632029287434\",\"crate_name\":\"enum_match_derive\"}"]
                            #[export_name = "{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"r: {}\",\"disambiguator\":\"8069176632029287434\",\"crate_name\":\"enum_match_derive\"}"]
                            static DEFMT_LOG_STATEMENT: u8 = 0;
                            &DEFMT_LOG_STATEMENT as *const u8 as u16
                        })
                    },
                );
                defmt::export::fmt(arg0);
                unsafe { defmt::export::release() }
            }
        }
    };
    let r = test_validate();
    match (&(r)) {
        (arg0) => {
            if {
                const CHECK: bool = {
                    const fn check() -> bool {
                        let module_path = "enum_match_derive".as_bytes();
                        if if 17usize > module_path.len() {
                            false
                        } else {
                            module_path[0usize] == 101u8 && module_path[1usize] == 110u8
                                && module_path[2usize] == 117u8
                                && module_path[3usize] == 109u8
                                && module_path[4usize] == 95u8
                                && module_path[5usize] == 109u8
                                && module_path[6usize] == 97u8
                                && module_path[7usize] == 116u8
                                && module_path[8usize] == 99u8
                                && module_path[9usize] == 104u8
                                && module_path[10usize] == 95u8
                                && module_path[11usize] == 100u8
                                && module_path[12usize] == 101u8
                                && module_path[13usize] == 114u8
                                && module_path[14usize] == 105u8
                                && module_path[15usize] == 118u8
                                && module_path[16usize] == 101u8
                                && if 17usize == module_path.len() {
                                    true
                                } else {
                                    module_path[17usize] == b':'
                                }
                        } {
                            return true;
                        }
                        false
                    }
                    check()
                };
                CHECK
            } {
                unsafe { defmt::export::acquire() };
                defmt::export::header(
                    &{
                        defmt::export::make_istr({
                            #[link_section = ".defmt.{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"r: {}\",\"disambiguator\":\"2479413202080893013\",\"crate_name\":\"enum_match_derive\"}"]
                            #[export_name = "{\"package\":\"armv6-m-examples\",\"tag\":\"defmt_info\",\"data\":\"r: {}\",\"disambiguator\":\"2479413202080893013\",\"crate_name\":\"enum_match_derive\"}"]
                            static DEFMT_LOG_STATEMENT: u8 = 0;
                            &DEFMT_LOG_STATEMENT as *const u8 as u16
                        })
                    },
                );
                defmt::export::fmt(arg0);
                unsafe { defmt::export::release() }
            }
        }
    };
    loop {}
}
enum Inner {
    One,
    Two(u16, u16),
}
impl Any for Inner {
    fn any() -> Self {
        match u8::any() {
            0 => Inner::One,
            _ => Inner::Two(u16::any(), u16::any()),
        }
    }
}
enum TestEnum {
    One,
    Two,
    Three(u16),
    Four(Inner),
    Five(u16),
}
impl Any for TestEnum {
    fn any() -> Self {
        let mut n = 1u8;
        symbolic(&mut n);
        match n {
            0 => TestEnum::One,
            1 => TestEnum::Two,
            2 => TestEnum::Three(u16::any()),
            3 => TestEnum::Four(Inner::any()),
            _ => TestEnum::Five(u16::any()),
        }
    }
}
enum TestEnum2 {
    One,
    Two,
    Three(u16),
    Four(u16),
    Five(u16),
}
impl symex_lib::Valid for TestEnum2 {
    #[inline(never)]
    fn is_valid(&self) -> bool {
        let input = &unsafe {
            let raw_pointer = &raw const self;
            core::ptr::read_volatile(raw_pointer as *const Self)
        };
        if let TestEnum2::One = input {
            true
        } else if let TestEnum2::Two = input {
            true
        } else if let TestEnum2::Three(t) = input {
            t.is_valid()
        } else if let TestEnum2::Four(t) = input {
            t.is_valid()
        } else if let TestEnum2::Five(t) = input {
            t.is_valid()
        } else {
            false
        }
    }
}
#[inline(never)]
#[no_mangle]
fn test_any() -> u16 {
    let input: TestEnum = any();
    let r = handle_test_enum(input);
    r
}
#[inline(never)]
#[no_mangle]
fn test_validate() -> u16 {
    let mut input: TestEnum2 = TestEnum2::One;
    symbolic(&mut input);
    assume(input.is_valid());
    let r = handle_test_enum2(input);
    r
}
#[inline(never)]
#[no_mangle]
fn handle_test_enum2(n: TestEnum2) -> u16 {
    match n {
        TestEnum2::One => 1,
        TestEnum2::Two => simple_if(2),
        TestEnum2::Three(v) => v,
        TestEnum2::Four(i1) => i1 + i1,
        TestEnum2::Five(v) => simple_if(v),
    }
}
#[inline(never)]
#[no_mangle]
fn handle_test_enum(n: TestEnum) -> u16 {
    match n {
        TestEnum::One => 1,
        TestEnum::Two => simple_if(2),
        TestEnum::Three(v) => v,
        TestEnum::Four(i) => {
            match i {
                Inner::One => 1,
                Inner::Two(a, b) => a + b,
            }
        }
        TestEnum::Five(v) => simple_if(v),
    }
}
#[inline(never)]
#[no_mangle]
fn simple_if(n: u16) -> u16 {
    if n == 3 { 1 } else if n == 6 { 5 } else { 2 }
}
