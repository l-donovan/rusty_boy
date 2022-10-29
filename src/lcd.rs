use pixels::{Pixels, PixelsBuilder, SurfaceTexture};
use winit::event_loop::EventLoop;
use winit::window::Window;

use crate::mem::MappedMemory;
use crate::util;

pub struct LCD {
    background_enabled: bool,
    obj_enabled: bool,
    obj_size: bool,
    background_tilemap: bool,
    background_tiles: bool,
    window_enabled: bool,
    window_tilemap: bool,
    lcd_enabled: bool,
    window: Window,
    pixels: Option<Pixels>,
}

const WIDTH: u32 = 160;
const HEIGHT: u32 = 144;
const SCALE: u32 = 8;

impl LCD {
    pub fn new(event_loop: &EventLoop<()>) -> LCD {
        let win = Window::new(&event_loop).unwrap();

        let mut lcd = LCD {
            background_enabled: false,
            obj_enabled: false,
            obj_size: false,
            background_tilemap: false,
            background_tiles: false,
            window_enabled: false,
            window_tilemap: false,
            lcd_enabled: false,
            window: win,
            pixels: None,
        };

        // let size = window.inner_size();
        let surface_texture = SurfaceTexture::new(WIDTH * SCALE, HEIGHT * SCALE, &lcd.window);

        let px = PixelsBuilder::new(WIDTH, HEIGHT, surface_texture)
            // .texture_format(pixels::wgpu::TextureFormat::R8Uint)
            .enable_vsync(false)
            .build();

        match px {
            Err(e) => {
                error!("Error: {}", e);
            }
            Ok(v) => {
                lcd.pixels = Some(v);
            }
        };

        return lcd;
    }

    pub fn set_lcdc(&mut self, val: u8) {
        self.background_enabled = util::bit_is_set(val, 0);
        self.obj_enabled = util::bit_is_set(val, 1);
        self.obj_size = util::bit_is_set(val, 2);
        self.background_tilemap = util::bit_is_set(val, 3);
        self.background_tiles = util::bit_is_set(val, 4);
        self.window_enabled = util::bit_is_set(val, 5);
        self.window_tilemap = util::bit_is_set(val, 6);
        self.lcd_enabled = util::bit_is_set(val, 7);
    }

    pub fn draw(&mut self, mem: &mut MappedMemory) {
        let row_start = 0;
        let rows = 18;

        let col_start = 0;
        let cols = 20;

        let px = match &mut self.pixels {
            Some(p) => p,
            None => {
                error!("Error: Failed to grab pixels reference");
                return;
            }
        };

        let frame = px.get_frame();

        for row in row_start..(row_start + rows) {
            for col in col_start..(col_start + cols) {
                let i = u16::from(mem.at(0x9800 + row * 32 + col));

                for m in 0..8 {
                    let lo = mem.at(0x8000 + i * 16 + 2 * m);
                    let hi = mem.at(0x8000 + i * 16 + 2 * m + 1);

                    for k in (0..=7).rev() {
                        let a = (lo >> k) & 1u8;
                        let b = (hi >> k) & 1u8;
                        let c = (b << 1) | a;
                        let v = match c {
                            0b00 => 0x00,
                            0b01 => 0x55,
                            0b10 => 0xaa,
                            0b11 => 0xff,
                            _ => 0x00,
                        };

                        let y = (((row - row_start) * 8) + m) as usize;
                        let x = (((col - col_start) * 8) + 7 - k) as usize;

                        let i: usize = 4 * (y * (WIDTH as usize) + x);

                        frame[i + 0] = v;
                        frame[i + 1] = v;
                        frame[i + 2] = v;
                        frame[i + 3] = 0xff;
                    }
                }
            }
        }

        match px.render() {
            Ok(()) => {}
            Err(e) => {
                error!("Error: {}", e);
            }
        };
    }
}
