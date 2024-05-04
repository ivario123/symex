#![no_std]
#![no_main]
#![feature(type_alias_impl_trait)]

#[rtic::app(
    device = rp2040_hal::pac,
    dispatchers = [TIMER_IRQ_1, UART0_IRQ]
)]
mod app {
    use rp_pico::XOSC_CRYSTAL_FREQ;

    //use defmt::*;
    use defmt_rtt as _;

    use rp2040_hal::{
        clocks,
        fugit::Instant,
        gpio,
        gpio::{
            bank0::{Gpio11, Gpio12, Gpio13, Gpio2, Gpio22, Gpio25, Gpio3},
            FunctionSio, PullDown, SioInput, SioOutput,
        },
        pac, Sio, Watchdog, I2C,
    };
    use rtic_sync::{
        channel::{Receiver, Sender},
        make_channel,
    };

    use core::mem::MaybeUninit;
    use embedded_hal::digital::v2::InputPin;
    use embedded_hal::digital::v2::{OutputPin, ToggleableOutputPin};
    use fugit::RateExtU32;
    use rtic_monotonics::{rp2040::*, Monotonic};

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
        debounce: bool,
    }

    #[local]
    struct Local {
        led: gpio::Pin<Gpio25, FunctionSio<SioOutput>, PullDown>,
        led_1: Led1,
        led_2: Led2,
        led_3: Led3,
        i2c: &'static mut I2CBus,
        button: gpio::Pin<Gpio22, FunctionSio<SioInput>, PullDown>,
        d_sender: Sender<'static, Instant<u64, 1, 1000000>, 1>,
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
        let rp2040_timer_token = rtic_monotonics::create_rp2040_monotonic_token!();
        // Configure the clocks, watchdog - The default is to generate a 125 MHz system clock
        Timer::start(ctx.device.TIMER, &mut ctx.device.RESETS, rp2040_timer_token); // default rp2040 clock-rate is 125MHz
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

        let button = gpioa.gpio22.into_pull_down_input();
        button.set_interrupt_enabled(gpio::Interrupt::EdgeHigh, true);
        button.set_interrupt_enabled(gpio::Interrupt::EdgeLow, true);

        let mut led_1 = gpioa.gpio13.into_push_pull_output();
        let mut led_2 = gpioa.gpio12.into_push_pull_output();
        let mut led_3 = gpioa.gpio11.into_push_pull_output();
        led_1.set_high().unwrap();
        led_2.set_low().unwrap();
        led_3.set_low().unwrap();

        // Init I2C pins
        let sda_pin = gpioa.gpio2.into_function::<gpio::FunctionI2C>();
        let scl_pin = gpioa.gpio3.into_function::<gpio::FunctionI2C>();

        // Init I2C itself, using MaybeUninit to overwrite the previously
        // uninitialized i2c_ctx variable without dropping its value
        // (i2c_ctx definined in init local resources above)
        let i2c_tmp: &'static mut _ = ctx.local.i2c_ctx.write(I2C::i2c1(
            ctx.device.I2C1,
            sda_pin,
            scl_pin,
            100.kHz(),
            &mut ctx.device.RESETS,
            &clocks.system_clock,
        ));

        // Spawn heartbeat task
        heartbeat::spawn().ok();

        // Create a channel for debouncing.
        let (d_sender, d_receiver) = make_channel!(Instant<u64, 1, 1000000>, 1);

        debounce::spawn(d_receiver).ok();

        // Return resources and timer
        (
            Shared { debounce: false },
            Local {
                led,
                led_1,
                led_2,
                led_3,
                i2c: i2c_tmp,
                button,
                d_sender,
            },
        )
    }

    #[task(priority = 2, shared = [debounce])]
    async fn debounce(
        mut ctx: debounce::Context,
        mut receiver: Receiver<'static, Instant<u64, 1, 1000000>, 1>,
    ) {
        while let Ok(delay) = receiver.recv().await {
            let now = Timer::now().ticks();
            defmt::info!(
                "message received at: {} delay until: {}",
                now,
                delay.ticks()
            );
            Timer::delay_until(delay).await;
            defmt::info!("check empty");
            if receiver.is_empty() {
                ctx.shared.debounce.lock(|d| {
                    *d = false;
                });
                defmt::info!("ready");
            } else {
                defmt::info!("channel not empty")
            }
        }
    }

    #[task(binds = IO_IRQ_BANK0, priority = 2, shared = [debounce], local = [led_1, led_2, led_3, button, state:u8 = 0, d_sender])]
    fn button_handler(mut ctx: button_handler::Context) {
        let now = Timer::now();
        defmt::info!("button handler");
        // read button state
        let button_state = ctx.local.button.is_high().unwrap();

        // clear interrupts
        ctx.local.button.clear_interrupt(gpio::Interrupt::EdgeLow);
        ctx.local.button.clear_interrupt(gpio::Interrupt::EdgeHigh);

        // check if interrupted because of bounce
        let mut should_return = false;
        ctx.shared.debounce.lock(|d| {
            if *d {
                defmt::info!("bounce detected");
                should_return = true;
            } else {
                *d = true;
            }
        });

        // return if bounce detected
        if should_return {
            return;
        }

        // check if pressed or released
        if button_state {
            // change state
            let s = ctx.local.state;
            *s += 1;
            if *s > 2 {
                *s = 0;
            }
            defmt::info!("button pressed: state {}, time: {}", *s, now.ticks());
            // update leds
            handle_leds(ctx.local.led_1, ctx.local.led_2, ctx.local.led_3, *s);
        } else {
            // do nothing if released
            defmt::info!("Button Low");
        }

        // add new delay to channel
        ctx.local.d_sender.try_send(now + 100.millis()).ok();
        defmt::info!("message sent");
    }

    #[no_mangle]
    #[inline(never)]
    fn handle_leds(led_1: &mut Led1, led_2: &mut Led2, led_3: &mut Led3, state: u8) {
        match state {
            0 => {
                led_1.set_high().unwrap();
                led_2.set_low().unwrap();
                led_3.set_low().unwrap();
            }
            1 => {
                led_1.set_low().unwrap();
                led_2.set_high().unwrap();
                led_3.set_low().unwrap();
            }
            2 => {
                led_1.set_low().unwrap();
                led_2.set_low().unwrap();
                led_3.set_high().unwrap();
            }
            _ => panic!(),
        }
    }

    #[task(priority = 1, local = [i2c, led])]
    async fn heartbeat(ctx: heartbeat::Context) {
        let mut next_blink = Timer::now();
        loop {
            // Flicker the built-in LED
            _ = ctx.local.led.toggle();
            defmt::info!("blink");

            next_blink += 500.millis();

            Timer::delay_until(next_blink).await;
        }
    }
}
