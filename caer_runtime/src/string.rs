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
    pub fn from_str(rt: &mut Runtime, s: &str) -> Self {
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(s.to_owned().into_boxed_str())) };
        Self {
            heap_header: HeapObject::string(),
            size: s.len() as u64,
            ptr: ptr.cast(),
        }
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
