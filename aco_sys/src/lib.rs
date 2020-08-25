#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[inline]
pub unsafe fn aco_get_co() -> *mut aco_t {
    aco_rshim_get_co()
}

#[inline]
pub unsafe fn aco_get_arg() -> *mut std::ffi::c_void {
    (*aco_rshim_get_co()).arg
}

#[inline]
pub unsafe fn aco_is_main_co() -> bool {
    (*aco_rshim_get_co()).main_co.is_null()
}

#[inline]
pub unsafe fn aco_yield() {
    let co = aco_rshim_get_co();
    assert!(!co.is_null());
    assert!(!(*co).main_co.is_null());
    acosw(co, (*co).main_co);
}

#[inline]
pub unsafe fn aco_exit() {
    let co = aco_rshim_get_co();
    (*co).is_end = 1;
    assert!((*(*co).share_stack).owner == co);
    (*(*co).share_stack).owner = std::ptr::null_mut();
    (*(*co).share_stack).align_validsz = 0;
    aco_yield();
    unreachable!();
}
