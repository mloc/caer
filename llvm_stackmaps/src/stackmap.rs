#[derive(Debug, Clone)]
pub struct StackMap {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub addr: u64,
    pub stack_size: u64,
    pub records: Vec<Record>,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub patch_point_id: u64,
    pub instruction_offset: u32,
    pub flags: u16,
    pub locations: Vec<Location>,
    pub live_outs: Vec<LiveOut>,
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub size: u16,
    pub pointer: LocationPointer,
}

#[derive(Debug, Clone, Copy)]
pub enum LocationPointer {
    Register { reg: u16 },
    Direct { addr: u64 },
    Indirect { reg: u16, offset: i32 },
    Constant(u64),
}

#[derive(Debug, Clone, Copy)]
pub struct LiveOut {
    pub reg: u16,
    pub size: u8,
}
