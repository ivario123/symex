#![no_std]
#![no_main]
#![feature(type_alias_impl_trait)]

#[rtic::app(
    device = rp2040_hal::pac,
    dispatchers = []
)]
mod app {
    use rp_pico::XOSC_CRYSTAL_FREQ;

    //use defmt::*;
    use defmt_rtt as _;

    use rp2040_hal::{
        clocks, fugit::Instant, gpio::{self, bank0::{Gpio11, Gpio12, Gpio13, Gpio2, Gpio22, Gpio25, Gpio3}, FunctionSio, PullDown, SioInput, SioOutput}, pac::{self, timer::alarm0}, timer::{Alarm0, Alarm1, Alarm2, Timer, Alarm}, Sio, Watchdog, I2C
    };
    use rtic_sync::{
        channel::{Receiver, Sender},
        make_channel,
    };

    use core::mem::MaybeUninit;
    use embedded_hal::digital::v2::InputPin;
    use embedded_hal::digital::v2::{OutputPin, ToggleableOutputPin};
    use rp2040_hal::fugit::RateExtU32;
    use rp2040_hal::fugit::ExtU32;
    //use rtic_monotonics::{rp2040::*, Monotonic};

    use panic_probe as _;

    type I2CBus = I2C<
        pac::I2C1,
        (
            gpio::Pin<Gpio2, gpio::FunctionI2C, PullDown>,
            gpio::Pin<Gpio3, gpio::FunctionI2C, PullDown>,
        ),
    >;

    type Led1 = gpio::Pin<Gpio13, FunctionSio<SioOutput>, PullDown>;
    type Led2 = gpio::Pin<Gpio12, FunctionSio<SioOutput>, PullDown>;
    type Led3 = gpio::Pin<Gpio11, FunctionSio<SioOutput>, PullDown>;

    #[shared]
    struct Shared {
        shared: u32,
        debounce: bool,
        alarm1: Alarm1,
    }

    #[local]
    struct Local {
        led: gpio::Pin<Gpio25, FunctionSio<SioOutput>, PullDown>,
        button: gpio::Pin<Gpio22, FunctionSio<SioInput>, PullDown>,
        alarm0: Alarm0,
        alarm2: Alarm2,
        led1: Led1,
    }

    #[init(local=[
        // Task local initialized resources are static
        // Here we use MaybeUninit to allow for initialization in init()
        // This enables its usage in driver initialization
        i2c_ctx: MaybeUninit<I2CBus> = MaybeUninit::uninit()
    ])]
    fn init(mut ctx: init::Context) -> (Shared, Local) {
        defmt::info!("init");
        // Initialize the interrupt for the RP2040 timer and obtain the token
        // proving that we have.
        //let rp2040_timer_token = rtic_monotonics::create_rp2040_monotonic_token!();
        // Configure the clocks, watchdog - The default is to generate a 125 MHz system clock
        //Timer::start(ctx.device.TIMER, &mut ctx.device.RESETS, rp2040_timer_token); // default rp2040 clock-rate is 125MHz
        let mut watchdog = Watchdog::new(ctx.device.WATCHDOG);
        let clocks = clocks::init_clocks_and_plls(
            XOSC_CRYSTAL_FREQ,
            ctx.device.XOSC,
            ctx.device.CLOCKS,
            ctx.device.PLL_SYS,
            ctx.device.PLL_USB,
            &mut ctx.device.RESETS,
            &mut watchdog,
        )
        .ok()
        .unwrap();

        let mut timer = Timer::new(ctx.device.TIMER, &mut ctx.device.RESETS, &clocks);
        let mut alarm0 = timer.alarm_0().unwrap();
        let mut alarm1 = timer.alarm_1().unwrap();
        let mut alarm2 = timer.alarm_2().unwrap();

        // schedule alarm 
        alarm0.enable_interrupt();
        alarm1.enable_interrupt();
        alarm2.enable_interrupt();
        alarm0.clear_interrupt();
        alarm1.clear_interrupt();
        alarm2.clear_interrupt();
        alarm0.schedule(500u32.millis());
        alarm2.schedule(1000u32.millis());

        // Init LED pin
        let sio = Sio::new(ctx.device.SIO);
        let gpioa = rp_pico::Pins::new(
            ctx.device.IO_BANK0,
            ctx.device.PADS_BANK0,
            sio.gpio_bank0,
            &mut ctx.device.RESETS,
        );
        let mut led = gpioa.led.into_push_pull_output();
        led.set_low().unwrap();
        let mut led1 = gpioa.gpio13.into_push_pull_output();

        let button = gpioa.gpio22.into_pull_down_input();
        button.set_interrupt_enabled(gpio::Interrupt::EdgeHigh, true);
        button.set_interrupt_enabled(gpio::Interrupt::EdgeLow, true);

        // Return resources and timer
        (Shared { shared: 0, debounce: true, alarm1 }, Local { led1, alarm2, alarm0, led, button })
    }

    #[task(binds = TIMER_IRQ_2, priority = 4, shared = [], local = [alarm2, led1])]
    fn alarm2_handler(mut ctx: alarm2_handler::Context) {
        ctx.local.alarm2.clear_interrupt();

        ctx.local.led1.toggle();

        ctx.local.alarm2.schedule(1000u32.millis());
    }

    #[task(binds = TIMER_IRQ_0, priority = 1, shared = [shared], local = [alarm0, led])]
    fn alarm0_handler(mut ctx: alarm0_handler::Context) {
        ctx.local.alarm0.clear_interrupt();
        let mut shared = ctx.shared.shared;

        shared.lock(|v| {
            if *v % 4 == 0 {
                ctx.local.led.toggle();
            }
        });

        ctx.local.alarm0.schedule(500u32.millis());
    }

    #[task(binds = TIMER_IRQ_1, priority = 3, shared = [alarm1, debounce], local = [])]
    fn debounce_button(mut ctx: debounce_button::Context) {
        let mut alarm = ctx.shared.alarm1;
        let mut debounce = ctx.shared.debounce;

        (alarm, debounce).lock(|alarm, debounce| {
            alarm.clear_interrupt();
            *debounce = true;
        });
    }

    #[task(binds = IO_IRQ_BANK0, priority = 2, shared = [alarm1, shared, debounce], local = [button])]
    fn button_handler(mut ctx: button_handler::Context) {
        ctx.local.button.clear_interrupt(gpio::Interrupt::EdgeHigh);
        ctx.local.button.clear_interrupt(gpio::Interrupt::EdgeLow);
        if ctx.local.button.is_high().unwrap() {
            let shared = ctx.shared.shared;
            let alarm1 = ctx.shared.alarm1;
            let debounce = ctx.shared.debounce;

            (debounce, shared, alarm1).lock(|debounce, shared, alarm1| {
                if *debounce {
                    *shared += 1;
                    alarm1.schedule(300u32.millis());
                    *debounce = false;
                }
            });
        }
    }
    // lock => write to 0xe000e180
    // unlock => write to 0xe000e100
}
