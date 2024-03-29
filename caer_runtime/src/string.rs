use std::ptr::NonNull;

use caer_types::id::INSTANCE_TYPE_ID_STRING;
use pinion::PinionData;

use crate::alloc::Alloc;
use crate::heap_object::HeapHeader;

#[derive(Debug, Clone, PinionData)]
#[repr(C)]
pub struct RtString {
    pub heap_header: HeapHeader,
    size: u64,
    ptr: NonNull<u8>,
}

impl RtString {
    pub fn as_str(&self) -> &str {
        unsafe {
            let slice = std::slice::from_raw_parts(self.ptr.as_ptr(), self.size as usize);
            std::str::from_utf8_unchecked(slice)
        }
    }

    // TODO: allocator?
    pub fn from_str(s: impl Into<Box<str>>) -> Self {
        let s = s.into();
        let size = s.len() as u64;
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(s)) };
        Self {
            heap_header: HeapHeader::string(),
            size,
            ptr: ptr.cast(),
        }
    }

    pub fn heapify(self, alloc: &Alloc) -> NonNull<Self> {
        alloc.alloc_emplace(self, INSTANCE_TYPE_ID_STRING)
    }
}

impl Drop for RtString {
    fn drop(&mut self) {
        let boxed = unsafe {
            Box::from_raw(std::slice::from_raw_parts_mut(
                self.ptr.as_ptr(),
                self.size as usize,
            ))
        };
        std::mem::drop(boxed);
    }
}

pub fn resolve_string<'a>(s: Option<NonNull<RtString>>) -> &'a str {
    match s {
        None => "",
        Some(ptr) => unsafe { ptr.as_ref().as_str() },
    }
}
