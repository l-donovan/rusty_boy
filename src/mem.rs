use memmap::{Mmap, MmapOptions};
use std::alloc::{alloc, dealloc, Layout};
use std::fs::File;
use std::ptr::null_mut;

use crate::mem_io::IoMemory;
use crate::mem_mmap::MmapWrapper;
use crate::mem_seg::{Mapping, MemCommand};

const MEM_ALIGN: usize = 0x0002 - 0x0000;
const MEM_SIZE: usize = 0xffff;

pub struct MappedMemory {
    mem: *mut u8,
    bootstrap: Option<Mapping>,
    mappings: Vec<Option<Mapping>>,
}

impl MappedMemory {
    pub fn new() -> MappedMemory {
        let mut mm = MappedMemory {
            mem: null_mut(),
            bootstrap: None,
            mappings: vec![],
        };

        mm.initialize_memory();
        mm.map_bootstrap("bootstrap.bin");
        mm.map_cartridge("dmg_test_prog_ver1.gb");
        mm.map_io();

        mm
    }

    fn initialize_memory(&mut self) {
        let layout = match Layout::from_size_align(MEM_ALIGN * MEM_SIZE, MEM_ALIGN) {
            Err(e) => {
                error!("Error: {}", e);
                return;
            }
            Ok(l) => l,
        };

        self.mem = unsafe { alloc(layout) };
    }

    pub fn at(&mut self, addr: u16) -> u8 {
        if let Some(ref mut bootstrap) = self.bootstrap {
            if addr <= 0xff {
                return match bootstrap.target.get(addr) {
                    None => {
                        error!("{:#06x} -> 0x????????????????", addr);
                        0
                    }
                    Some(p) => {
                        trace!("{:#06x} -> {:#18p}", addr, p);
                        *p
                    }
                };
            }
        }

        for maybe_mapping in self.mappings.iter_mut() {
            if let Some(ref mut mapping) = maybe_mapping {
                let (offset, underflow) = addr.overflowing_sub(mapping.start);

                if !underflow && offset <= mapping.size {
                    return match mapping.target.get(offset) {
                        None => {
                            error!("{:#06x} -> 0x????????????????", addr);
                            0
                        }
                        Some(p) => {
                            trace!("{:#06x} -> {:#18p}", addr, p);
                            *p
                        }
                    };
                }
            }
        }

        unsafe {
            trace!("{:#06x} -> {:#18p}", addr, self.mem.add(addr as usize));
            *self.mem.add(addr as usize)
        }
    }

    pub fn set(&mut self, addr: u16, val: u8) {
        for maybe_mapping in self.mappings.iter_mut() {
            if let Some(ref mut mapping) = maybe_mapping {
                let (offset, underflow) = addr.overflowing_sub(mapping.start);

                if !underflow && offset <= mapping.size {
                    match mapping.target.set(offset, val) {
                        MemCommand::NOP => {}
                        MemCommand::UnmapBootstrap => self.unmap_bootstrap(),
                    }

                    return;
                }
            }
        }

        unsafe {
            trace!("{:#06x} -> {:#18p}", addr, self.mem.add(addr as usize));
            *self.mem.add(addr as usize) = val;
        };
    }

    fn map_bootstrap(&mut self, filename: &str) {
        let file = match File::options().write(false).read(true).open(filename) {
            Err(e) => {
                error!("Error loading bootstrap: {}", e);
                return;
            }
            Ok(f) => f,
        };

        let mmap: Mmap = match unsafe { MmapOptions::new().map(&file) } {
            Err(e) => {
                error!("Error: {}", e);
                return;
            }
            Ok(m) => m,
        };

        let mapping = Mapping {
            start: 0x0000,
            size: 0x00ff,
            target: Box::new(MmapWrapper(mmap)),
        };

        self.bootstrap = Some(mapping);
    }

    pub fn map_cartridge(&mut self, filename: &str) {
        let file = match File::options().write(false).read(true).open(filename) {
            Err(e) => {
                error!("Error loading bootstrap: {}", e);
                return;
            }
            Ok(f) => f,
        };

        let mmap: Mmap = match unsafe { MmapOptions::new().map(&file) } {
            Err(e) => {
                error!("Error: {}", e);
                return;
            }
            Ok(m) => m,
        };

        let mut mapping = Mapping {
            start: 0x0000,
            size: 0x3fff,
            target: Box::new(MmapWrapper(mmap)),
        };

        mapping.target.init();

        self.mappings.push(Some(mapping));
    }

    fn map_io(&mut self) {
        let mut mapping = Mapping {
            start: 0xff00,
            size: 0x004b,
            target: Box::new(IoMemory { mem: null_mut() }),
        };

        mapping.target.init();

        self.mappings.push(Some(mapping));
    }

    pub fn unmap(&mut self, addr: u16) {
        let index = match self.mappings.iter().position(|x| match x {
            Some(y) => y.start == addr,
            None => false,
        }) {
            None => {
                error!(
                    "Attempted to unmap memory at {:#06x}, but no memory was mapped",
                    addr
                );
                return;
            }
            Some(i) => i,
        };

        self.mappings.remove(index);
    }

    pub fn unmap_bootstrap(&mut self) {
        self.bootstrap = None;
    }

    pub fn debug_mappings(&mut self) {
        debug!("MAPPINGS:");

        for thing in self.mappings.iter() {
            if let Some(ref mm) = thing {
                debug!("- ALIVE: {:04x} - {:04x}", mm.start, mm.start + mm.size);
            } else {
                debug!("- DEAD");
            }
        }
    }
}

impl Drop for MappedMemory {
    fn drop(&mut self) {
        self.bootstrap = None;
        //self.unmap(0xff00);

        let layout = match Layout::from_size_align(MEM_ALIGN * MEM_SIZE, MEM_ALIGN) {
            Err(e) => {
                error!("Error: {}", e);
                return;
            }
            Ok(l) => l,
        };

        unsafe { dealloc(self.mem, layout) };
    }
}
