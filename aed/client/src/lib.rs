extern crate common;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;
extern crate serde_cbor;

extern crate futures;
extern crate tokio;
extern crate tokio_io;
extern crate bytes;

mod client;

use std::os::raw::{c_char, c_int};
use std::ffi::{CStr, CString};
use std::cell::RefCell;
use std::sync::RwLock;
use std::thread;
use tokio::prelude::*;
use tokio_io::codec::length_delimited;
use std::sync::Mutex;
use std::mem::replace;
use common::messages;
use futures::sync::mpsc;

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
    return p;
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

#[no_mangle]
pub extern "C" fn hello(n: c_int, v: *const *const c_char) -> *const c_char {
    let args = norm_args(n, v);
    let s = CString::new(format!("{}: {:?}", n, args)).unwrap();
    return_byond(s)
}

#[no_mangle]
pub extern "C" fn dibby_setup(_n: c_int, _v: *const *const c_char) -> *const c_char {
    {
        let mut client = CLIENT.lock().unwrap();
        if let None = *client {
            *client = Some(client::Client::new())
        } else {
            return return_byond(CString::new("existing client").unwrap());
        }
    }

    return EMPTY_RET.as_ptr();
}

#[no_mangle]
pub extern "C" fn dibby_shutdown(_n: c_int, _v: *const *const c_char) -> *const c_char {
    let mut lock = CLIENT.lock().unwrap();
    if let Some(_) = *lock {
        let client = replace(&mut *lock, None).unwrap();
        client.shutdown();
    } else {
        return return_byond(CString::new("no client").unwrap());
    }
    return EMPTY_RET.as_ptr();
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
    if n != 1 {
        return return_byond(CString::new("bad number of args").unwrap());
    }

    let args = norm_args(n, v);

    let msg = serde_json::from_slice(args[0].as_bytes()).unwrap();

    let mut client = CLIENT.lock().unwrap();
    if let Some(ref mut client) =  *client {
        client.send(msg);
        EMPTY_RET.as_ptr()
    } else {
        return_byond(CString::new("no client").unwrap())
    }
}
