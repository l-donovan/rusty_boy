use memmap::{Mmap, MmapOptions};
use std::alloc::{alloc, dealloc, Layout};
use std::fs::File;
use std::ptr::null_mut;

const MEM_ALIGN: usize = 0x0002 - 0x0000;
const MEM_SIZE: usize = 0xffff;

struct Mapping {
    start: u16,
    size: u16,
    target: Mmap,
}

pub struct MappedMemory {
    mem: *mut u8,
    mappings: Vec<Mapping>,
}

impl MappedMemory {
    pub fn new() -> MappedMemory {
        let mut mm = MappedMemory {
            mem: null_mut(),
            mappings: vec![],
        };

        mm.initialize_memory();
        mm.map_rom();

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

    pub fn mem_at(&self, addr: u16) -> u8 {
        for mapping in self.mappings.iter() {
            let (offset, underflow) = addr.overflowing_sub(mapping.start);

            if !underflow && offset <= mapping.size {
                return match mapping.target.get(offset as usize) {
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

        unsafe {
            trace!("{:#06x} -> {:#18p}", addr, self.mem.add(addr as usize));
            *self.mem.add(addr as usize)
        }
    }

    pub fn mem_set(&self, addr: u16, val: u8) {
        for mapping in self.mappings.iter() {
            let (offset, underflow) = addr.overflowing_sub(mapping.start);

            if !underflow && offset <= mapping.size {
                // File-backed mapped memory can't be written to like normal memory
                warn!("Writing to mapped memory is currently unsupported");
                return;
            }
        }

        unsafe {
            trace!("{:#06x} -> {:#18p}", addr, self.mem.add(addr as usize));
            *self.mem.add(addr as usize) = val;
        };
    }

    fn map_rom(&mut self) {
        let file = match File::options()
            .write(false)
            .read(true)
            .open("bootstrap.bin")
        {
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

        self.mappings.push(Mapping {
            start: 0x0000,
            size: 0x00ff,
            target: mmap,
        })
    }

    fn unmap_rom(&mut self) {
        let index = match self.mappings.iter().position(|x| x.start == 0x0000) {
            None => {
                error!("Attempted to unmap ROM, but ROM was not mapped");
                return;
            }
            Some(i) => i,
        };

        self.mappings.remove(index);
    }
}

impl Drop for MappedMemory {
    fn drop(&mut self) {
        self.unmap_rom();

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
