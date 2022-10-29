use crate::mem_seg::{MemCommand, MemorySegment};
use crate::{cpu::CPU, mem::MappedMemory};
use std::alloc::{alloc, Layout};

const MEM_ALIGN: usize = 0x0002 - 0x0000;
const MEM_SIZE: usize = 0x004b;

pub struct IoMemory {
    pub mem: *mut u8,
}

impl IoMemory {
    fn peek(&mut self, addr: u16) -> u8 {
        match addr {
            0x0042 => 0x00,
            0x0044 => 0x90,
            _ => {
                warn!("Unknown IO peek address 0x{:04x}", addr);
                0
            }
        }
    }

    fn poke(&mut self, addr: u16, val: u8) -> MemCommand {
        match addr {
            0x0050 => MemCommand::UnmapBootstrap,
            _ => {
                warn!("Unknown IO poke address 0x{:04x}", addr);
                MemCommand::NOP
            }
        }
    }
}

impl MemorySegment for IoMemory {
    fn init(&mut self) {
        let layout = match Layout::from_size_align(MEM_ALIGN * MEM_SIZE, MEM_ALIGN) {
            Err(e) => {
                error!("Error: {}", e);
                return;
            }
            Ok(l) => l,
        };

        self.mem = unsafe { alloc(layout) };
    }

    fn get(&mut self, addr: u16) -> Option<&u8> {
        let val = self.peek(addr);
        let p = unsafe { self.mem.add(addr as usize) };
        unsafe { *p = val }

        trace!("IO {:#06x} = {:#04x}", addr, val);

        unsafe {
            // TODO: There has to be a better way to do this
            return Some(&*p);
        };
    }

    fn set(&mut self, addr: u16, val: u8) -> MemCommand {
        self.poke(addr, val)
    }
}
