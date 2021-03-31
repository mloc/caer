use bytes::{Bytes, BytesMut};
use std::{net::SocketAddr, pin::Pin};
use aed_common::messages;
use futures::channel::mpsc;
use std::io;
use futures::{Future, Stream, Sink, StreamExt, SinkExt, FutureExt, future, TryStream, TryStreamExt};
use tokio::net::{TcpStream};
use tokio_util::codec::{Framed, LengthDelimitedCodec};
use futures::task::Poll;

pub struct Client {
    runtime: tokio::runtime::Runtime,
    sendq: mpsc::UnboundedSender<messages::Client>,
    recvq: mpsc::UnboundedReceiver<messages::Server>,
}

impl Client {
    pub fn new(host: &str) -> Self {
        let mut rt = tokio::runtime::Runtime::new().unwrap();

        let (out_send, out_recv) = mpsc::unbounded();
        let (in_send, in_recv) = mpsc::unbounded();

        rt.spawn(
            tokio::net::TcpStream::connect(host.parse::<SocketAddr>().unwrap()).then(|f| { future::ready(f
                .map(|sock| {
                    println!("{:?}", sock);
                    Self::process(sock, out_recv, in_send);
                })
                .map_err(|e| {
                    println!("connect error: {:?}", e);
                }))
            }
        ));

        //let task = rt.spawn(in_recv);

        Client {
            runtime: rt,
            sendq: out_send,
            recvq: in_recv,
        }
    }

    pub fn shutdown(self) {
        self.runtime.shutdown_background()
    }

    pub fn send(&self, msg: messages::Client) {
        self.sendq.unbounded_send(msg).unwrap(); // TODO errors
    }

    pub fn poll_recv(&mut self) -> Option<messages::Server> {
        let waker = futures::task::noop_waker_ref();
        let mut cx = std::task::Context::from_waker(waker);

        let poll = Pin::new(&mut self.recvq).poll_next(&mut cx); // TODO errors
        match poll {
            Poll::Ready(t) => t,
            Poll::Pending => None,
        }
    }

    fn process(
        sock: TcpStream,
        out_recv: mpsc::UnboundedReceiver<messages::Client>,
        in_send: mpsc::UnboundedSender<messages::Server>,
    ) {
        let framed = Framed::new(sock, LengthDelimitedCodec::new());
        let (send, recv) = framed.split();

        tokio::spawn(Self::process_reader(recv, in_send));
        tokio::spawn(Self::process_writer(send, out_recv));
    }

    fn process_reader(
        reader: impl TryStream<Ok = BytesMut, Error = io::Error>,
        in_send: mpsc::UnboundedSender<messages::Server>,
    ) -> impl Future<Output = ()> {
        reader
            .map_err(|e| {
                println!("recv error: {:?}", e);
            })
            .and_then(|x| {
                let v = serde_cbor::de::from_slice(&x).map_err(|e| {
                    println!("decode error: {:?}", e);
                });
                future::ready(v)
            })
            .forward(in_send.sink_map_err(|e| {
                println!("receive channel error: {:?}", e);
            }))
            .then(|_| future::ready(()))
    }

    fn process_writer(
        writer: impl Sink<Bytes, Error = io::Error>,
        out_recv: mpsc::UnboundedReceiver<messages::Client>,
    ) -> impl Future<Output = ()> {
        out_recv
            .map(Ok)
            .and_then(|x| {
                future::ready(serde_cbor::ser::to_vec(&x)
                    .map(Bytes::from)
                    .map_err(|e| {
                        println!("encode error: {:?}", e);
                    }))
            })
            .forward(writer.sink_map_err(|e| {
                println!("send error: {:?}", e);
            }))
            .then(|_| future::ready(()))
    }
}
