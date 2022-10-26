use crate::mem_seg::{MemorySegment, MemCommand};
use memmap::Mmap;

pub struct MmapWrapper(pub Mmap);

impl MemorySegment for MmapWrapper {
    fn init(&mut self) {}

    fn get(&mut self, addr: u16) -> Option<&u8> {
        self.0.get(addr as usize)
    }

    fn set(&mut self, _addr: u16, _val: u8) -> MemCommand {
        warn!("Writing to mapped memory is currently unsupported");
        MemCommand::NOP
    }
}
