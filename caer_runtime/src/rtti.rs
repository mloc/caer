use std::fmt::Debug;
use std::hash::Hash;
use std::ptr::NonNull;

use pinion::PinionData;

use crate::heap_object::HeapHeader;
use crate::vtable;

/*#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RttiHeader {
    pub vptr: &'static vtable::Entry,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct RttiWrap<T>
where
    T: Debug + Clone,
{
    pub header: RttiHeader,
    pub obj: T,
}*/

#[derive(Debug, Clone, Copy, PinionData)]
#[repr(C)]
pub struct RttiRef {
    pub vptr: &'static vtable::Entry,
    pub ptr: NonNull<HeapHeader>,
}

impl RttiRef {
    // temporary, TODO: clean up rr creation
    /// SAFETY: Creating two RttiRefs with identical ptr but distinct vptr causes UB. vptr must
    /// point to a vtable entry compatible with ptr.
    pub unsafe fn new(vptr: &'static vtable::Entry, ptr: NonNull<HeapHeader>) -> Self {
        Self { vptr, ptr }
    }
}

impl PartialEq for RttiRef {
    fn eq(&self, other: &Self) -> bool {
        assert_eq!(self.vptr as *const _, other.vptr as *const _);
        self.ptr == other.ptr
    }
}

impl Eq for RttiRef {}

impl Hash for RttiRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}
