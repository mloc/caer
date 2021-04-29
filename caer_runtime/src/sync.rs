use std::{iter::FusedIterator, sync::mpsc::{Receiver, RecvTimeoutError, TryRecvError}};
use std::time::Duration;

use aed_server::server;

type Message = (server::ClientId, aed_common::messages::Client);

pub struct SyncServer {
    server: server::Server,
    stream: Receiver<Message>,
}

impl SyncServer {
    pub fn setup() -> Self {
        let (server, stream) = server::Server::start(&"0.0.0.0:2939".parse().unwrap());
        Self { server, stream }
    }

    pub fn iter_messages_timeout(
        &self, timeout: Duration,
    ) -> MessagesIter {
        MessagesIter::new_timeout(&self.stream, timeout)
    }

    pub fn iter_messages_nonblocking(
        &self,
    ) -> MessagesIter {
        MessagesIter::new_nonblocking(&self.stream)
    }
}

pub struct MessagesIter<'a> {
    stream: &'a Receiver<Message>,
    state: IterState,
}

impl<'a> MessagesIter<'a> {
    fn new_timeout(stream: &'a Receiver<Message>, timeout: Duration) -> Self {
        Self {
            stream,
            state: IterState::BlockingTimeout(timeout),
        }
    }

    fn new_nonblocking(stream: &'a Receiver<Message>) -> Self {
        Self{
            stream,
            state: IterState::Streaming,
        }
    }
}

impl<'a> Iterator for MessagesIter<'a> {
    type Item = Message;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            IterState::BlockingTimeout(timeout) => {
                // TODO: use deadline api when stabler
                match self.stream.recv_timeout(timeout) {
                    Ok(m) => {
                        self.state = IterState::Streaming;
                        Some(m)
                    },
                    Err(RecvTimeoutError::Timeout) => {
                        self.state = IterState::Done;
                        None
                    },
                    Err(RecvTimeoutError::Disconnected) => {
                        panic!("server disconnected!");
                    },
                }
            },
            IterState::Streaming => {
                match self.stream.try_recv() {
                    Ok(m) => {
                        Some(m)
                    }
                    Err(TryRecvError::Empty) => {
                        self.state = IterState::Done;
                        None
                    },
                    Err(TryRecvError::Disconnected) => {
                        panic!("server disconnected!");
                    },
                }
            },
            IterState::Done => None,
        }
    }
}

impl<'a> FusedIterator for MessagesIter<'a> {}

enum IterState {
    BlockingTimeout(Duration),
    Streaming,
    Done,
}
