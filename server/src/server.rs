use futures::sync::mpsc;
use futures::Future;
use futures::stream::Stream;
use tokio;
use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;
use tokio_io::codec::length_delimited;
use indexmap::IndexMap;
use std::net::SocketAddr;
use std::sync::{Arc, RwLock};
use common::messages;
use bytes::BytesMut;
use std::io;
use serde_cbor;

#[derive(Debug, Clone)]
pub struct Server {
    pub shared: Arc<ServerShared>,
}

#[derive(Debug)]
pub struct ServerShared {
    pub source: mpsc::UnboundedReceiver<messages::Server>,
    sink: mpsc::UnboundedSender<messages::Client>,

    clients: RwLock<IndexMap<SocketAddr, mpsc::UnboundedSender<BytesMut>>>,
}

impl Server {
    pub fn start(addr: &SocketAddr) -> (Self, impl Future<Item = (), Error = ()> + Send) {
        let (source_w, source_r) = mpsc::unbounded();
        let (sink_w, sink_r) = mpsc::unbounded();

        let server = Server {
            shared: Arc::new(ServerShared {
                source: source_r,
                sink: sink_w,

                clients: RwLock::new(IndexMap::new()),
            }),
        };

        let server_pass = server.clone();
        let listen = TcpListener::bind(addr).unwrap();
        let future = listen
            .incoming()
            .for_each(move |sock| {
                let server = server_pass.clone();
                println!("got connection!");

                server.process_connection(sock);

                Ok(())
            })
            .map_err(|e| {
                println!("accept error: {:?}", e);
            })
            .into_future();

        (server, future)
    }

    fn process_connection(&self, sock: TcpStream) {
        let peer_addr = sock.peer_addr().unwrap();
        let (recv, send) = sock.split();

        let reader = length_delimited::FramedRead::new(recv);
        let writer = length_delimited::FramedWrite::new(send);

        tokio::spawn(self.process_reader(&peer_addr, reader));
        tokio::spawn(self.process_writer(&peer_addr, writer));
    }

    fn process_reader(&self, peer_addr: &SocketAddr, reader: impl Stream<Item = BytesMut, Error = io::Error>) -> impl Future<Item = (), Error = ()> {
        let peer_addr = peer_addr.clone();
        let shared = self.shared.clone();

        reader
            .map_err(|e| {
                println!("recv error: {:?}", e);
            })
            .and_then(|x| {
                serde_cbor::de::from_slice(&x).map_err(|e| {
                    println!("decode error: {:?}", e);
                })
            })
            .forward(self.shared.sink.clone().sink_map_err(|e| {
                println!("receive channel error: {:?}", e);
            }))
            .then(move |_| {
                println!("{} disconnected", peer_addr);
                let mut clients = shared.clients.write().unwrap();
                clients.remove(&peer_addr);
                Ok(())
            })
    }

    fn process_writer(&self, peer_addr: &SocketAddr, writer: impl Sink<SinkItem = BytesMut, SinkError = io::Error>) -> impl Future<Item = (), Error = ()> {
        let (client_w, client_r) = mpsc::unbounded();

        {
            let mut clients = self.shared.clients.write().unwrap();
            clients.insert(peer_addr.clone(), client_w);
        }

        let peer_addr = peer_addr.clone();
        println!("{} start", peer_addr);

        client_r
            .map_err(|e| {
                println!("client send channel error: {:?}", e);
            })
            .forward(writer.sink_map_err(|e| {
                println!("send error: {:?}", e);
            }))
            .then(|_| {
                Ok(())
            })
    }

    pub fn send(&self, msg: &messages::Client) {
        let enc = BytesMut::from(serde_cbor::ser::to_vec(msg).unwrap()); // todo error type
        for client in self.shared.clients.read().unwrap().values() {
            client.unbounded_send(enc.clone());
        }
    }
}
