extern crate pretty_env_logger;
#[macro_use]
extern crate log;

mod cpu;
mod mem;

fn main() {
    pretty_env_logger::init();

    // Create our CPU
    let mut cpu = cpu::CPU::new();
    cpu.boot();

    for _ in 1..1_000_000 {
        cpu.cycle();
    }
}
