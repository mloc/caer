use bytes::BytesMut;
use common::messages;
use futures::sync::mpsc;
use futures::{Async, Future, Stream};
use serde_cbor;
use std::io;
use tokio;
use tokio::net::TcpStream;
use tokio::prelude::*;
use tokio_io::codec::length_delimited;

pub struct Client {
    runtime: tokio::runtime::Runtime,
    sendq: mpsc::UnboundedSender<messages::Client>,
    recvq: mpsc::UnboundedReceiver<messages::Server>,
}

impl Client {
    pub fn new() -> Self {
        let mut rt = tokio::runtime::Runtime::new().unwrap();

        let (out_send, out_recv) = mpsc::unbounded();
        let (in_send, in_recv) = mpsc::unbounded();

        rt.spawn(
            tokio::net::TcpStream::connect(&"127.0.0.1:2939".parse().unwrap())
                .map(|sock| {
                    println!("{:?}", sock);
                    Self::process(sock, out_recv, in_send);
                })
                .map_err(|e| {
                    println!("connect error: {:?}", e);
                }),
        );

        Client {
            runtime: rt,
            sendq: out_send,
            recvq: in_recv,
        }
    }

    pub fn shutdown(self) {
        self.runtime.shutdown_now().wait().unwrap()
    }

    pub fn send(&self, msg: messages::Client) {
        self.sendq.unbounded_send(msg).unwrap(); // TODO errors
    }

    pub fn poll_recv(&mut self) -> Option<messages::Server> {
        let poll = self.recvq.poll().unwrap(); // TODO errors
        match poll {
            Async::Ready(t) => t,
            Async::NotReady => None,
        }
    }

    fn process(
        sock: TcpStream,
        out_recv: mpsc::UnboundedReceiver<messages::Client>,
        in_send: mpsc::UnboundedSender<messages::Server>,
    ) {
        let (recv, send) = sock.split();

        let reader = length_delimited::FramedRead::new(recv);
        let writer = length_delimited::FramedWrite::new(send);

        tokio::spawn(Self::process_reader(reader, in_send));
        tokio::spawn(Self::process_writer(writer, out_recv));
    }

    fn process_reader(
        reader: impl Stream<Item = BytesMut, Error = io::Error>,
        in_send: mpsc::UnboundedSender<messages::Server>,
    ) -> impl Future<Item = (), Error = ()> {
        reader
            .map_err(|e| {
                println!("recv error: {:?}", e);
            })
            .and_then(|x| {
                serde_cbor::de::from_slice(&x).map_err(|e| {
                    println!("decode error: {:?}", e);
                })
            })
            .forward(in_send.sink_map_err(|e| {
                println!("receive channel error: {:?}", e);
            }))
            .then(|_| Ok(()))
    }

    fn process_writer(
        writer: impl Sink<SinkItem = BytesMut, SinkError = io::Error>,
        out_recv: mpsc::UnboundedReceiver<messages::Client>,
    ) -> impl Future<Item = (), Error = ()> {
        out_recv
            .map_err(|e| {
                println!("client send channel error: {:?}", e);
            })
            .and_then(|x| {
                serde_cbor::ser::to_vec(&x)
                    .map(|v| BytesMut::from(v))
                    .map_err(|e| {
                        println!("encode error: {:?}", e);
                    })
            })
            .forward(writer.sink_map_err(|e| {
                println!("send error: {:?}", e);
            }))
            .then(|_| Ok(()))
    }
}
