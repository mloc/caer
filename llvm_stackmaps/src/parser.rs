use crate::stackmap::*;
use byteorder::ByteOrder;
use std::marker::PhantomData;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("cannot parse stackmap format version {0}")]
    UnsupportedVersion(u8),
    #[error("reserved field at {offset} expected to be 0, actually {actual}")]
    NonZeroReserved { offset: usize, actual: u64 },
    #[error("padding at {offset} expected to be 0, actually {actual}")]
    NonZeroPadding { offset: usize, actual: u32 },
    #[error("internal parser error: misaligned from 8 by {misalignment} bytes, expected 0 or 4")]
    BadAlignment { misalignment: usize },
    #[error("read unsupported location type {0}")]
    UnsupportedLocationType(u8),
    #[error("got negative offset {0} into constant table")]
    NegativeConstantOffset(i32),
    #[error("offset {0} is out of bounds of constant table")]
    ConstantTableOverflow(i32),
    #[error("unexpectedly hit end of input data while reading {0} bytes")]
    UnexpectedEof(usize),
    #[error("found {actual} records, but functions expect {functions_sum}")]
    BadRecordCount { actual: u64, functions_sum: u64 },
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a, BO: ByteOrder> {
    data: &'a [u8],
    pos: usize,
    _phantom: PhantomData<BO>,
}

impl<'a, BO: ByteOrder + 'a> Parser<'a, BO> {
    pub fn parse(data: &'a [u8]) -> Result<StackMap> {
        let mut parser = Parser {
            data,
            pos: 0,
            _phantom: PhantomData::<BO>,
        };

        parser.parse_stackmap()
    }

    fn parse_stackmap(&mut self) -> Result<StackMap> {
        let version = self.read_u8()?;
        if version != 3 {
            return Err(ParseError::UnsupportedVersion(version));
        }
        self.expect_reserved_u8()?;
        self.expect_reserved_u16()?;

        let num_functions = self.read_u32()?;
        let num_constants = self.read_u32()?;
        let num_records = self.read_u32()?;

        let mut record_count_sum = 0;
        let mut functions_info = Vec::with_capacity(num_functions as usize);
        for _ in 0..num_functions {
            let addr = self.read_u64()?;
            let stack_size = self.read_u64()?;
            let record_count = self.read_u64()?;
            record_count_sum += record_count;
            functions_info.push((addr, stack_size, record_count as usize));
        }
        if record_count_sum != num_records.into() {
            return Err(ParseError::BadRecordCount {
                actual: num_records.into(),
                functions_sum: record_count_sum,
            });
        }

        let mut constants = Vec::with_capacity(num_constants as usize);
        for _ in 0..num_constants {
            constants.push(self.read_u64()?);
        }

        let mut records = Vec::with_capacity(num_records as usize);
        for _ in 0..num_records {
            records.push(self.parse_record(&constants)?);
        }
        let records = records.into_boxed_slice();

        let mut cur_record = 0;

        let mut functions = Vec::with_capacity(num_functions as usize);
        for (addr, stack_size, num_records) in functions_info {
            functions.push(Function {
                addr,
                stack_size,
                records: records[cur_record..cur_record + num_records].to_vec(),
            });
            cur_record += num_records;
        }

        Ok(StackMap { functions })
    }

    fn parse_record(&mut self, constants: &[u64]) -> Result<Record> {
        // in this spec this alignment padding is at the bottom of record
        // but, since records are guaranteeed to be aligned, we can put this here
        // we can't do the alignment check at the bottom of record, as it'll EOF if llvm decides to
        // cut off the stackmap section there.
        self.pad_align()?;

        let patch_point_id = self.read_u64()?;
        let instruction_offset = self.read_u32()?;
        let flags = self.read_u16()?;

        let num_locations = self.read_u16()?;
        let mut locations = Vec::with_capacity(num_locations as usize);
        for _ in 0..num_locations {
            locations.push(self.parse_location(constants)?);
        }
        self.pad_align()?;
        self.expect_padding_u16()?;

        let num_live_outs = self.read_u16()?;
        let mut live_outs = Vec::with_capacity(num_live_outs as usize);
        for _ in 0..num_live_outs {
            live_outs.push(self.parse_live_out()?);
        }

        Ok(Record {
            patch_point_id,
            instruction_offset,
            flags,
            locations,
            live_outs,
        })
    }

    fn parse_location(&mut self, constants: &[u64]) -> Result<Location> {
        let pointer_type = self.read_u8()?;
        self.expect_reserved_u8()?;
        let size = self.read_u16()?;
        let reg = self.read_u16()?;
        self.expect_reserved_u16()?;
        let offset = self.read_i32()?;

        let pointer = match pointer_type {
            0x01 => LocationPointer::Register { reg },
            0x02 => LocationPointer::Direct {
                addr: ((reg as i32) + offset) as u64,
            },
            0x03 => LocationPointer::Indirect { reg, offset },
            0x04 => LocationPointer::Constant(offset as u64),
            0x05 => {
                if offset < 0 {
                    return Err(ParseError::NegativeConstantOffset(offset));
                }
                let constant;
                if let Some(c) = constants.get(offset as usize) {
                    constant = *c;
                } else {
                    return Err(ParseError::ConstantTableOverflow(offset));
                }
                LocationPointer::Constant(constant)
            }
            unsupported => return Err(ParseError::UnsupportedLocationType(unsupported)),
        };

        Ok(Location { size, pointer })
    }

    fn parse_live_out(&mut self) -> Result<LiveOut> {
        let reg = self.read_u16()?;
        self.expect_reserved_u8()?;
        let size = self.read_u8()?;

        Ok(LiveOut { reg, size })
    }

    // don't look below here it's ugly
    // still waiting on specialization

    fn pad_align(&mut self) -> Result<()> {
        match self.pos % 8 {
            0 => Ok(()),
            4 => self.expect_padding_u32(),
            misalignment => Err(ParseError::BadAlignment { misalignment }),
        }
    }

    fn read_n(&mut self, n: usize) -> Result<&[u8]> {
        let (new_pos, over) = self.pos.overflowing_add(n);
        if over || new_pos >= self.data.len() {
            return Err(ParseError::UnexpectedEof(n));
        }
        let res = &self.data[self.pos..new_pos];
        self.pos = new_pos;
        Ok(res)
    }

    fn read_u8(&mut self) -> Result<u8> {
        Ok(self.read_n(1)?[0])
    }

    fn read_u16(&mut self) -> Result<u16> {
        Ok(BO::read_u16(self.read_n(2)?))
    }

    fn read_u32(&mut self) -> Result<u32> {
        Ok(BO::read_u32(self.read_n(4)?))
    }

    fn read_u64(&mut self) -> Result<u64> {
        Ok(BO::read_u64(self.read_n(8)?))
    }

    fn read_i32(&mut self) -> Result<i32> {
        Ok(BO::read_i32(self.read_n(4)?))
    }

    fn expect_reserved_u8(&mut self) -> Result<()> {
        let n = self.read_u8()?;
        if n != 0 {
            Err(ParseError::NonZeroReserved {
                offset: self.pos,
                actual: n as u64,
            })
        } else {
            Ok(())
        }
    }

    fn expect_reserved_u16(&mut self) -> Result<()> {
        let n = self.read_u16()?;
        if n != 0 {
            Err(ParseError::NonZeroReserved {
                offset: self.pos,
                actual: n as u64,
            })
        } else {
            Ok(())
        }
    }

    fn expect_padding_u16(&mut self) -> Result<()> {
        let n = self.read_u16()?;
        if n != 0 {
            Err(ParseError::NonZeroPadding {
                offset: self.pos,
                actual: n as u32,
            })
        } else {
            Ok(())
        }
    }

    fn expect_padding_u32(&mut self) -> Result<()> {
        let n = self.read_u32()?;
        if n != 0 {
            Err(ParseError::NonZeroPadding {
                offset: self.pos,
                actual: n as u32,
            })
        } else {
            Ok(())
        }
    }
}
