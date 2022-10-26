pub enum MemCommand {
    NOP,
    UnmapBootstrap,
}

pub trait MemorySegment {
    fn init(&mut self);
    fn get(&mut self, addr: u16) -> Option<&u8>;
    fn set(&mut self, addr: u16, val: u8) -> MemCommand;
}

pub struct Mapping {
    pub start: u16,
    pub size: u16,
    pub target: Box<dyn MemorySegment>,
}
