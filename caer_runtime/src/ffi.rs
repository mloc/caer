use std::convert::TryInto;
use std::convert::TryFrom;
use std::slice::from_raw_parts;
use std::ptr::NonNull;
use num_traits::Zero;

#[repr(C)]
#[derive(Debug)]
pub struct FfiArray<T, I = u64> {
    len: I,
    data: NonNull<T>,
}

impl<T, I> FfiArray<T, I> where I: Copy + Zero + Eq + TryFrom<usize> {
    pub fn empty() -> Self {
        Self {
            len: I::zero(),
            data: NonNull::dangling(),
        }
    }

    /// # Safety
    /// lol
    pub unsafe fn with_len(data: NonNull<T>, len: I) -> Self {
        Self {
            len,
            data,
        }
    }

    // TODO: fix lifetimes, make into From or something
    /// # Safety
    /// lol
    pub unsafe fn from_vec(vec: &Vec<T>) -> Self {
        Self {
            len: vec.len().try_into().unwrap_or_else(|_| panic!("oh no")),
            data: NonNull::new(vec.as_ptr() as *mut _).unwrap(),
        }
    }

    pub fn len(&self) -> I {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == I::zero()
    }
}

impl<T, I> FfiArray<T, I>
where
    I: Copy + TryInto<usize>,
    <I as TryInto<usize>>::Error: std::fmt::Debug, // meh
{
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        unsafe { from_raw_parts(self.data.as_ptr(), self.len.try_into().expect("array size too big to cast to usize")) }
    }
}
