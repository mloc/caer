use std::ptr::NonNull;

use crate::heap_object::HeapObject;
use crate::runtime::Runtime;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct RtString {
    pub heap_header: HeapObject,
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
            heap_header: HeapObject::string(),
            size,
            ptr: ptr.cast(),
        }
    }

    pub fn heapify(self) -> NonNull<Self> {
        unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(self))) }
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
