extern crate pretty_env_logger;
use winit::event_loop::EventLoop;
#[macro_use]
extern crate log;

mod cpu;
mod lcd;
mod mem;
mod mem_io;
mod mem_mmap;
mod mem_seg;
mod util;

fn l(v: u8) -> &'static str {
    match v {
        0b00 => " ",
        0b01 => "░",
        0b10 => "▒",
        0b11 => "▓",
        _ => "?",
    }
}

fn dump_vram(mem: &mut mem::MappedMemory) {
    let row_start = 0;
    let rows = 18;

    let col_start = 0;
    let cols = 20;

    for row in row_start..=(row_start + rows) {
        for col in col_start..=(col_start + cols) {
            let i = u16::from(mem.at(0x9800 + row * 32 + col));

            for m in 0..8 {
                let lo = mem.at(0x8000 + i * 16 + 2 * m);
                let hi = mem.at(0x8000 + i * 16 + 2 * m + 1);

                print!(
                    "\x1b[{};{}H",
                    ((row - row_start) * 8) + 1 + m,
                    ((col - col_start) * 8) + 1
                );

                for k in (0..=7).rev() {
                    let a = (lo >> k) & 1u8;
                    let b = (hi >> k) & 1u8;
                    let c = (b << 1) | a;
                    print!("{}", l(c));
                }
            }
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let event_loop = EventLoop::new();

    // Create our system
    let mut mem = mem::MappedMemory::new();
    let mut cpu = cpu::CPU::new(&event_loop);
    let mut lcd = lcd::LCD::new(&event_loop);

    for _ in 1..1_000_000 {
        cpu.cycle(&mut mem);
    }

    lcd.draw(&mut mem);
    mem.debug_mappings();
    event_loop.run(|_, _, _| {});
}
