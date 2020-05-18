use cstub_bindgen_macro::expose_c_stubs;
use serde::{Serialize, Deserialize};

use crate::string_table;
use crate::runtime::Runtime;

use std::fmt;
use std::mem;

// the runtime threading here is bad, very bad.
// this is an artifact of originally using a TLS stringtable
// honestly, this type should just be a newtype index now, with all logic on stringtable
// TODO: replace

#[repr(C)]
#[derive(Debug, PartialEq, PartialOrd, Copy, Clone, Serialize, Deserialize)]
pub struct DmString(u64);

#[expose_c_stubs(rt_string)]
impl DmString {
    fn from_utf8(rt: &mut Runtime, bytes: *const u8, n: u32) -> u64 {
        // v bad, TODO: better string conversion, maybe stop checking if valid? all literals should
        // be valid utf-8 already
        let sl = unsafe { std::slice::from_raw_parts(bytes, n as usize) };
        let s = std::str::from_utf8(sl).unwrap().into();

        DmString::new(rt, s).id()
    }

    pub fn concat_into(rt: &mut Runtime, o: &mut DmString, l: &DmString, r: &DmString) {
        mem::forget(mem::replace(o, DmString::concat(rt, l, r)));
    }
}

impl DmString {
    pub fn new(rt: &mut Runtime, s: String) -> Self {
        let id = rt.string_table.put(s);

        Self(id)
    }

    #[inline(always)]
    pub fn id(&self) -> u64 {
        self.0
    }

    pub fn get_str<'a>(&self, rt: &'a Runtime) -> &'a str {
        rt.string_table.get(self.id())
    }

    pub fn concat(rt: &mut Runtime, l: &DmString, r: &DmString) -> DmString {
        DmString::new(rt, format!("{}{}", l.get_str(rt), r.get_str(rt)))
    }

    pub fn is_empty(&self) -> bool {
        self.id() == 0
    }
}
