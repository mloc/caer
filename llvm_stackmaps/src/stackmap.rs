#[derive(Debug)]
pub struct StackMap {
    pub functions: Vec<Function>,
    pub records: Vec<Record>,
}

#[derive(Debug)]
pub struct Function {
    pub addr: u64,
    pub stack_size: u64,
    pub record_count: u64,
}

#[derive(Debug)]
pub struct Record {
    pub patch_point_id: u64,
    pub instruction_offset: u32,
    pub flags: u16,
    pub locations: Vec<Location>,
    pub live_outs: Vec<LiveOut>,
}

#[derive(Debug)]
pub struct Location {
    pub size: u16,
    pub pointer: LocationPointer,
}

#[derive(Debug)]
pub enum LocationPointer {
    Register { reg: u16 },
    Direct { addr: u64 },
    Indirect { reg: u16, offset: i32 },
    Constant(u64),
}

#[derive(Debug)]
pub struct LiveOut {
    pub reg: u16,
    pub size: u8,
}
