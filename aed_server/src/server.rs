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
use aed_common::messages;
use bytes::BytesMut;
use std::io;
use serde_cbor;
use snowflake::ProcessUniqueId;

#[derive(Debug, Clone)]
pub struct Server {
    shared: Arc<ServerShared>,
}

#[derive(Debug)]
struct ServerShared {
    sink: mpsc::UnboundedSender<(ProcessUniqueId, messages::Client)>,

    pub clients: RwLock<IndexMap<ProcessUniqueId, mpsc::UnboundedSender<BytesMut>>>,
    addrs: RwLock<IndexMap<ProcessUniqueId, SocketAddr>>,
}

impl Server {
    pub fn start(addr: &SocketAddr) -> (Self, impl Stream<Item = (ProcessUniqueId, messages::Client), Error = ()>, impl Future<Item = (), Error = ()> + Send) {
        let (sink, source) = mpsc::unbounded();

        let server = Server {
            shared: Arc::new(ServerShared {
                sink,

                clients: RwLock::new(IndexMap::new()),
                addrs: RwLock::new(IndexMap::new()),
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

        (server, source, future)
    }

    fn process_connection(&self, sock: TcpStream) {
        let peer_addr = sock.peer_addr().unwrap();
        let (recv, send) = sock.split();

        let reader = length_delimited::FramedRead::new(recv);
        let writer = length_delimited::FramedWrite::new(send);

        let id = ProcessUniqueId::new();

        tokio::spawn(self.process_reader(id, reader));
        tokio::spawn(self.process_writer(id, writer));
    }

    fn process_reader(&self, id: ProcessUniqueId, reader: impl Stream<Item = BytesMut, Error = io::Error>) -> impl Future<Item = (), Error = ()> {
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
            .map(move |x| {
                (id, x)
            })
            .forward(self.shared.sink.clone().sink_map_err(|e| {
                println!("receive channel error: {:?}", e);
            }))
            .then(move |_| {
                println!("{} disconnected", id);
                let mut clients = shared.clients.write().unwrap();
                clients.remove(&id);
                Ok(())
            })
    }

    fn process_writer(&self, id: ProcessUniqueId, writer: impl Sink<SinkItem = BytesMut, SinkError = io::Error>) -> impl Future<Item = (), Error = ()> {
        let (client_w, client_r) = mpsc::unbounded();

        {
            let mut clients = self.shared.clients.write().unwrap();
            clients.insert(id, client_w);
        }

        println!("{} start", id);

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

    pub fn send(&self, id: ProcessUniqueId, msg: &messages::Server) {
        let enc = BytesMut::from(serde_cbor::ser::to_vec(msg).unwrap()); // TODO error type
        let clients = self.shared.clients.read().unwrap();
        if let Some(chan) = clients.get(&id) {
            chan.unbounded_send(enc.clone()).unwrap();
        } else {
            // TODO errors
        }
    }

    pub fn send_all(&self, msg: &messages::Server) {
        let enc = BytesMut::from(serde_cbor::ser::to_vec(msg).unwrap()); // TODO error type
        for client in self.shared.clients.read().unwrap().values() {
            client.unbounded_send(enc.clone()).unwrap();
        }
    }
}
