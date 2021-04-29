use std::fmt::Debug;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use std::{io, sync};

use aed_common::messages;
use bytes::{Bytes, BytesMut};
use futures::channel::mpsc;
use futures::task::Poll;
use futures::{
    future, Future, FutureExt, Sink, SinkExt, Stream, StreamExt, TryStream, TryStreamExt,
};
use indexmap::IndexMap;
use snowflake::ProcessUniqueId;
use tokio::net::{TcpListener, TcpStream};
use tokio_util::codec::{Framed, LengthDelimitedCodec};

#[derive(Debug, Clone)]
pub struct Server {
    shared: Arc<ServerShared>,
}

pub type ClientId = ProcessUniqueId;

#[derive(Debug)]
struct ServerShared {
    runtime: Arc<tokio::runtime::Runtime>,
    sink: sync::Mutex<sync::mpsc::Sender<(ClientId, messages::Client)>>,

    pub clients: RwLock<IndexMap<ProcessUniqueId, mpsc::UnboundedSender<Bytes>>>,
    addrs: RwLock<IndexMap<ProcessUniqueId, SocketAddr>>,
}

impl Server {
    pub fn start(addr: &SocketAddr) -> (Self, sync::mpsc::Receiver<(ProcessUniqueId, messages::Client)>) {
        let rt = Arc::new(tokio::runtime::Runtime::new().unwrap());
        let (sink, source) = sync::mpsc::channel();

        let server = Server {
            shared: Arc::new(ServerShared {
                runtime: rt.clone(),
                sink: sync::Mutex::new(sink),

                clients: RwLock::new(IndexMap::new()),
                addrs: RwLock::new(IndexMap::new()),
            }),
        };
        println!("server starting on {}", addr);

        let server_pass = server.clone();
        let addr = addr.to_owned();
        rt.spawn(async move {
            let listen = TcpListener::bind(addr).await.unwrap();
            tokio::spawn(async move {
                loop {
                    let (socket, _) = listen.accept().await.unwrap();
                    let server = server_pass.clone();
                    tokio::spawn(async move {
                        server.process_connection(socket);
                    });
                }
            });
        });
        /*let future = listen
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
        */

        (server, source)
    }

    fn process_connection(&self, sock: TcpStream) {
        let peer_addr = sock.peer_addr().unwrap();
        let framed = Framed::new(sock, LengthDelimitedCodec::new());
        let (send, recv) = framed.split();

        let id = ProcessUniqueId::new();
        println!("got connection from {}, id: {}", peer_addr, id);

        let rf = self.process_reader(id, recv);

        tokio::spawn(rf);
        tokio::spawn(self.process_writer(id, send));
    }

    fn process_reader(
        &self, id: ProcessUniqueId, reader: impl TryStream<Ok = BytesMut, Error = io::Error>,
    ) -> impl Future<Output = ()> {
        let shared = self.shared.clone();
        let sink = shared.sink.lock().unwrap().clone();

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
            .map_ok(move |x| (id, x))
            .try_for_each(move |x| {
                sink.send(x).unwrap();
                // TODO: bubble error
                future::ready(Ok(()))
            })
            .then(move |_| {
                println!("{} disconnected", id);
                {
                    let mut clients = shared.clients.write().unwrap();
                    clients.remove(&id);
                }
                future::ready(())
            })
    }

    fn process_writer(
        &self, id: ProcessUniqueId, writer: impl Sink<Bytes, Error = impl Debug>,
    ) -> impl Future<Output = ()> {
        let (client_w, client_r) = mpsc::unbounded();

        {
            let mut clients = self.shared.clients.write().unwrap();
            clients.insert(id, client_w);
        }

        println!("{} start", id);

        client_r
            .map(Ok)
            .forward(writer.sink_map_err(|e| {
                println!("send error: {:?}", e);
            }))
            .then(|_| future::ready(()))
    }

    pub fn send(&self, id: ProcessUniqueId, msg: &messages::Server) {
        let enc = BytesMut::from(serde_cbor::ser::to_vec(msg).unwrap().as_slice()); // TODO error type
        let clients = self.shared.clients.read().unwrap();
        if let Some(chan) = clients.get(&id) {
            chan.unbounded_send(enc.into()).unwrap();
        } else {
            // TODO errors
        }
    }

    pub fn send_all(&self, msg: &messages::Server) {
        let enc = BytesMut::from(serde_cbor::ser::to_vec(msg).unwrap().as_slice()); // TODO error type
        for client in self.shared.clients.read().unwrap().values() {
            client.unbounded_send(enc.clone().into()).unwrap();
        }
    }
}
