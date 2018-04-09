#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;
extern crate serde_cbor;

use std::os::raw::{c_char, c_int};
use std::ffi::{CStr, CString};
use std::cell::RefCell;
use std::sync::RwLock;
use std::thread;

lazy_static! {
//    static ref SENDQ:
//        RwLock<Option<futures::sync::mpsc::UnboundedSender<rpc::ClientMsg>>>
//        = RwLock::new(None);
//    static ref RECVQ:
//        RwLock<Option<futures::sync::mpsc::UnboundedReceiver<rpc::ServerMsg>>>
//        = RwLock::new(None);

    static ref EMPTY_RET: CString = CString::new("").unwrap();
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
pub extern "C" fn setup(n: c_int, v: *const *const c_char) -> *const c_char {
    thread::spawn(|| {
        /*
        let rpc_client =
            rpc_grpc::DibyClient::new_plain("localhost", 2977, Default::default()).unwrap();

        let (in_send, in_recv) = futures::sync::mpsc::unbounded();
        *(SENDQ.write().unwrap()) = Some(in_send);
        let (out_send, out_recv) = futures::sync::mpsc::unbounded();
        *(RECVQ.write().unwrap()) = Some(out_recv);

        let in_recv = in_recv.map_err(|()| grpc::Error::Other("send queue error"));
        let out_send = out_send.map_err(|()| grpc::Error::Other("recv queue error"));

        let req_stream = grpc::StreamingRequest::new(in_recv);
        let resp_stream = rpc_client
            .serve(grpc::RequestOptions::new(), req_stream)
            .no_metadata(out_send);

        resp_stream.wait().unwrap();
        */
    });

    return EMPTY_RET.as_ptr();
}
