//! Utilities for parsing DWARF-encoded data streams.
//! See <http://www.dwarfstd.org>,
//! DWARF-4 standard, Section 7 - "Data Representation"
//! Lifted from the rust compiler source code, commit ef92009c1dbe2750f1d24a6619b827721fb49749
//! Copyright the rust project's contributors: https://github.com/rust-lang/rust/blob/master/COPYRIGHT

#![allow(non_upper_case_globals)]

#[cfg(test)]
mod tests;

use core::mem;

pub const DW_EH_PE_omit: u8 = 0xFF;
pub const DW_EH_PE_absptr: u8 = 0x00;

pub const DW_EH_PE_uleb128: u8 = 0x01;
pub const DW_EH_PE_udata2: u8 = 0x02;
pub const DW_EH_PE_udata4: u8 = 0x03;
pub const DW_EH_PE_udata8: u8 = 0x04;
pub const DW_EH_PE_sleb128: u8 = 0x09;
pub const DW_EH_PE_sdata2: u8 = 0x0A;
pub const DW_EH_PE_sdata4: u8 = 0x0B;
pub const DW_EH_PE_sdata8: u8 = 0x0C;

pub const DW_EH_PE_pcrel: u8 = 0x10;
pub const DW_EH_PE_textrel: u8 = 0x20;
pub const DW_EH_PE_datarel: u8 = 0x30;
pub const DW_EH_PE_funcrel: u8 = 0x40;
pub const DW_EH_PE_aligned: u8 = 0x50;

pub const DW_EH_PE_indirect: u8 = 0x80;

pub struct DwarfReader {
    pub ptr: *const u8,
}

#[repr(C, packed)]
struct Unaligned<T>(T);

impl DwarfReader {
    pub fn new(ptr: *const u8) -> DwarfReader {
        DwarfReader { ptr }
    }

    // DWARF streams are packed, so e.g., a u32 would not necessarily be aligned
    // on a 4-byte boundary. This may cause problems on platforms with strict
    // alignment requirements. By wrapping data in a "packed" struct, we are
    // telling the backend to generate "misalignment-safe" code.
    pub unsafe fn read<T: Copy>(&mut self) -> T {
        let Unaligned(result) = *(self.ptr as *const Unaligned<T>);
        self.ptr = self.ptr.add(mem::size_of::<T>());
        result
    }

    // ULEB128 and SLEB128 encodings are defined in Section 7.6 - "Variable
    // Length Data".
    pub unsafe fn read_uleb128(&mut self) -> u64 {
        let mut shift: usize = 0;
        let mut result: u64 = 0;
        let mut byte: u8;
        loop {
            byte = self.read::<u8>();
            result |= ((byte & 0x7F) as u64) << shift;
            shift += 7;
            if byte & 0x80 == 0 {
                break;
            }
        }
        result
    }

    pub unsafe fn read_sleb128(&mut self) -> i64 {
        let mut shift: usize = 0;
        let mut result: u64 = 0;
        let mut byte: u8;
        loop {
            byte = self.read::<u8>();
            result |= ((byte & 0x7F) as u64) << shift;
            shift += 7;
            if byte & 0x80 == 0 {
                break;
            }
        }
        // sign-extend
        if shift < 8 * mem::size_of::<u64>() && (byte & 0x40) != 0 {
            result |= (!0 as u64) << shift;
        }
        result as i64
    }

    pub unsafe fn read_encoded(&mut self, encoding: u8) -> Result<usize, ()>{
        if encoding == DW_EH_PE_omit {
            return Err(());
        }

        // DW_EH_PE_aligned implies it's an absolute pointer value
        if encoding == DW_EH_PE_aligned {
            self.ptr = round_up(self.ptr as usize, mem::size_of::<usize>())? as *const u8;
            return Ok(self.read::<usize>());
        }

        let mut result = match encoding & 0x0F {
            DW_EH_PE_absptr => self.read::<usize>(),
            DW_EH_PE_uleb128 => self.read_uleb128() as usize,
            DW_EH_PE_udata2 => self.read::<u16>() as usize,
            DW_EH_PE_udata4 => self.read::<u32>() as usize,
            DW_EH_PE_udata8 => self.read::<u64>() as usize,
            DW_EH_PE_sleb128 => self.read_sleb128() as usize,
            DW_EH_PE_sdata2 => self.read::<i16>() as usize,
            DW_EH_PE_sdata4 => self.read::<i32>() as usize,
            DW_EH_PE_sdata8 => self.read::<i64>() as usize,
            _ => return Err(()),
        };

        result += match encoding & 0x70 {
            DW_EH_PE_absptr => 0,
            // relative to address of the encoded value, despite the name
            DW_EH_PE_pcrel => self.ptr as usize,
            // LLVM will never emit funcrel, textrel or datarel for the ptrs we care about, and
            // they're fiddly to implement. just error.
            DW_EH_PE_funcrel | DW_EH_PE_textrel | DW_EH_PE_datarel => return Err(()),
            // unhandled
            _ => return Err(()),
        };

        if encoding & DW_EH_PE_indirect != 0 {
            result = *(result as *const usize);
        }

        Ok(result)
    }
}

#[inline]
fn round_up(unrounded: usize, align: usize) -> Result<usize, ()> {
    if align.is_power_of_two() { Ok((unrounded + align - 1) & !(align - 1)) } else { Err(()) }
}
