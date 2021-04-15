//use futures::Async;
use futures::stream::{Fuse, Stream};
use snowflake::ProcessUniqueId;
use futures::StreamExt;
use aed_common::messages;
use futures::task::Poll;
use futures::Future;
use crate::server;

// cribbed from futures tests, used as we have no way to propagate notifications to DM
/*fn notify_noop() -> NotifyHandle {
    struct Noop;

    impl Notify for Noop {
        fn notify(&self, _id: usize) {}
    }

    const NOOP : &Noop = &Noop;

    NotifyHandle::from(NOOP)
}*/

pub struct Runtime {
    server: server::Server,

    msg_in: Box<dyn Stream<Item = (ProcessUniqueId, messages::Client)> + Unpin>,

    proc: unsafe extern fn(),
}

//#[expose_c_stubs(runtime)]
/*impl Runtime {
    async fn setup(proc: unsafe extern fn()) -> Self {
        let (server, msg_in) = server::Server::start(&"0.0.0.0:2939".parse().unwrap()).await;

        let msg_in: Box<dyn Stream<Item = (ProcessUniqueId, messages::Client)> + Unpin> = Box::new(msg_in.fuse());

        Self {
            server,
            msg_in,
            proc,
        }
    }

    /*fn proc_msg(&mut self) {
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
    }*/
}

impl Runtime {
    /*fn poll_msg(&mut self) -> Option<(ProcessUniqueId, messages::Client)> {
        let n = (*self.msg_in).next();
        let poll = n.poll().unwrap(); // TODO errors
        match poll {
            Poll::Ready(t) => t,
            Poll::Pending => None,
        }
    }*/
}*/
