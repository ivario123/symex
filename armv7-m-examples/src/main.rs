//! main.rs

#![deny(unsafe_code)]
#![no_main]
#![no_std]

use nrf52840_hal as _;
use panic_halt as _;
#[rtic::app(device = nrf52840_hal::pac, dispatchers = [TIMER0])]
mod app {
    use matrs::predule::*;
    use nrf52840_hal as hal;
    use rtt_target::{rprintln, rtt_init_print};

    #[shared]
    struct Shared {}

    #[local]
    struct Local {}

    #[init]
    fn init(ctx: init::Context) -> (Shared, Local, init::Monotonics) {
        let _clocks = hal::clocks::Clocks::new(ctx.device.CLOCK).enable_ext_hfosc();
        rtt_init_print!();
        let m1: Matrix<u32, 2, 3> = [[1, 0, 10], [1, 0, 0]].into();
        let m2: Matrix<u32, 3, 2> = [[2, 0], [1, 0], [1, 1]].into();
        let res = m1.clone() * m2.clone();
        foo::spawn().unwrap();
        rprintln!("{:?}*{:?} = {:?}", m1, m2, res);

        (Shared {}, Local {}, init::Monotonics())
    }

    #[task]
    #[inline(never)]
    fn foo(_: foo::Context) {
        cortex_m::asm::nop();
    }

    #[idle]
    fn idle(_cx: idle::Context) -> ! {
        //rprintln!("idle");

        //panic!("panic");

        //#[allow(unreachable_code)]
        loop {
            continue;
        }
    }
}
