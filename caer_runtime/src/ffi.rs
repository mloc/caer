use std::convert::TryInto;
use std::slice::from_raw_parts;

#[repr(C)]
pub struct FFIArray<T, I = u64> {
    len: I,
    data: *const T,
}

impl<T, I> FFIArray<T, I> {
    pub unsafe fn with_len(data: *const T, len: I) -> Self {
        Self {
            len,
            data,
        }
    }
}

impl<T, I> FFIArray<T, I>
where
    I: Copy + TryInto<usize>,
    <I as TryInto<usize>>::Error: std::fmt::Debug, // meh
{
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        unsafe { from_raw_parts(self.data, self.len.try_into().expect("array size too big to cast to usize")) }
    }
}
