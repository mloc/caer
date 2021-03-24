use futures::Async;
use futures::stream::{Fuse, Stream};
use futures::executor::{NotifyHandle, Notify};
use snowflake::ProcessUniqueId;
use aed_common::messages;
use crate::server;

// cribbed from futures tests, used as we have no way to propagate notifications to DM
fn notify_noop() -> NotifyHandle {
    struct Noop;

    impl Notify for Noop {
        fn notify(&self, _id: usize) {}
    }

    const NOOP : &Noop = &Noop;

    NotifyHandle::from(NOOP)
}

pub struct Runtime {
    server: server::Server,
    runtime: tokio::runtime::Runtime,

    msg_in: Fuse<Box<Stream<Item = (ProcessUniqueId, messages::Client), Error = ()>>>,

    proc: unsafe extern fn(),
}

//#[expose_c_stubs(runtime)]
impl Runtime {
    fn setup(proc: unsafe extern fn()) -> Self {
        let (server, msg_in, fut) = server::Server::start(&"0.0.0.0:2939".parse().unwrap());

        let msg_in: Box<Stream<Item = (ProcessUniqueId, messages::Client), Error = ()>> = Box::new(msg_in);

        let mut runtime = tokio::runtime::Runtime::new().unwrap();
        runtime.spawn(fut);

        Self {
            server: server,
            runtime: runtime,
            msg_in: msg_in.fuse(),
            proc: proc,
        }
    }

    fn proc_msg(&mut self) {
        match self.poll_msg() {
            None => {},
            Some((id, msg)) => {
                match msg {
                    messages::Client::Message(s) => {
                        unsafe { (self.proc)() }
                    }
                }
            },
        }
    }
}

impl Runtime {
    fn poll_msg(&mut self) -> Option<(ProcessUniqueId, messages::Client)> {
        let poll = self.msg_in.poll().unwrap(); // TODO errors
        match poll {
            Async::Ready(t) => t,
            Async::NotReady => None,
        }
    }
}
