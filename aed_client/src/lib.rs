#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_slice)]

#[macro_use]
extern crate lazy_static;

mod client;

use std::os::raw::{c_char, c_int};
use std::ffi::{CStr, CString};
use std::cell::RefCell;
use std::sync::Mutex;
use std::mem::replace;

lazy_static! {
    static ref EMPTY_RET: CString = CString::new("").unwrap();

    static ref CLIENT: Mutex<Option<client::Client>> = Mutex::new(None);
}

fn return_byond(s: CString) -> *const c_char {
    thread_local! {
        static RET: RefCell<Option<CString>> = RefCell::new(None);
    }

    let p = s.as_ptr();
    RET.with(|c| c.replace(Some(s)));
    p
}

fn norm_args(n: c_int, v: *const *const c_char) -> Vec<CString> {
    let n = n as isize;
    let mut out = Vec::with_capacity(n as usize);
    for i in 0..n {
        unsafe {
            out.push(CStr::from_ptr(*(v.offset(i))).to_owned());
        }
    }

    out
}

fn grab_args<'a, const N: usize>(n: c_int, v: *const *const c_char) -> Option<[&'a CStr; N]> {
    if n != N as c_int {
        return None
    }

    let dummy = EMPTY_RET.as_c_str();
    let mut o = [dummy; N];

    for i in 0..N {
        unsafe {
            o[i] = CStr::from_ptr(*(v.offset(i as _)));
        }
    }

    Some(o)
}

macro_rules! grab_args_or {
    ( $n:expr , $v:expr , $e:expr ) => {
        match grab_args($n, $v) {
            Some(x) => x,
            None => return return_byond(CString::new($e).unwrap()),
        }
    }
}

#[no_mangle]
pub extern "C" fn hello(n: c_int, v: *const *const c_char) -> *const c_char {
    let [greeting, second] = grab_args_or!(n, v, "error");
    let s = CString::new(format!("{}: {:?} {:?}", n, greeting, second)).unwrap();
    return_byond(s)
}

#[no_mangle]
pub extern "C" fn dibby_setup(n: c_int, v: *const *const c_char) -> *const c_char {
    let [host] = grab_args_or!(n, v, "bad number of args");

    {
        let mut client = CLIENT.lock().unwrap();
        if client.is_none() {
            *client = Some(client::Client::new(host.to_str().unwrap()))
        } else {
            return return_byond(CString::new("existing client").unwrap());
        }
    }

    EMPTY_RET.as_ptr()
}

#[no_mangle]
pub extern "C" fn dibby_shutdown(_n: c_int, _v: *const *const c_char) -> *const c_char {
    let mut lock = CLIENT.lock().unwrap();
    if lock.is_some() {
        let client = replace(&mut *lock, None).unwrap();
        client.shutdown();
    } else {
        return return_byond(CString::new("no client").unwrap());
    }

    EMPTY_RET.as_ptr()
}

#[no_mangle]
pub extern "C" fn dibby_recv(_n: c_int, _v: *const *const c_char) -> *const c_char {
    let mut client = CLIENT.lock().unwrap();
    if let Some(ref mut client) =  *client {
        match client.poll_recv() {
            Some(msg) => {
                let json = serde_json::to_vec(&msg).unwrap();
                return_byond(CString::new(json).unwrap())
            },
            None => EMPTY_RET.as_ptr()
        }
    } else {
        return_byond(CString::new("no client").unwrap())
    }
}

#[no_mangle]
pub extern "C" fn dibby_send(n: c_int, v: *const *const c_char) -> *const c_char {
    let [arg] = grab_args_or!(n, v, "bad number of args");

    let msg = serde_json::from_slice(arg.to_bytes()).unwrap();

    let mut client = CLIENT.lock().unwrap();
    if let Some(ref mut client) =  *client {
        client.send(msg);
        EMPTY_RET.as_ptr()
    } else {
        return_byond(CString::new("no client").unwrap())
    }
}
