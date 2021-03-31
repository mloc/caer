use futures::channel::mpsc;
use futures::Future;
use futures::future;
use futures::TryStream;
use futures::SinkExt;
use futures::FutureExt;
use std::fmt::Debug;
//use futures::TryFutureExt;
use aed_common::messages;
use bytes::{BytesMut, Bytes};
use futures::Sink;
use futures::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use indexmap::IndexMap;
use snowflake::ProcessUniqueId;
use std::io;
use std::net::SocketAddr;
use std::sync::{Arc, RwLock};
use tokio::net::{TcpListener, TcpStream};
use tokio_util::codec::{Framed, LengthDelimitedCodec};

#[derive(Debug, Clone)]
pub struct Server {
    shared: Arc<ServerShared>,
}

#[derive(Debug)]
struct ServerShared {
    sink: mpsc::UnboundedSender<(ProcessUniqueId, messages::Client)>,

    pub clients: RwLock<IndexMap<ProcessUniqueId, mpsc::UnboundedSender<Bytes>>>,
    addrs: RwLock<IndexMap<ProcessUniqueId, SocketAddr>>,
}

impl Server {
    pub async fn start(
        addr: &SocketAddr,
    ) -> (
        Self,
        impl Stream<Item = (ProcessUniqueId, messages::Client)>,
    ) {
        let (sink, source) = mpsc::unbounded();

        let server = Server {
            shared: Arc::new(ServerShared {
                sink,

                clients: RwLock::new(IndexMap::new()),
                addrs: RwLock::new(IndexMap::new()),
            }),
        };

        let server_pass = server.clone();
        let listen = TcpListener::bind(addr).await.unwrap();
        tokio::spawn(async move {
            loop {
                let (socket, e) = listen.accept().await.unwrap();
                let server = server_pass.clone();
                tokio::spawn(async move {
                    server.process_connection(socket);
                });
            }
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

        let rf = self.process_reader(id, recv);

        tokio::spawn(rf);
        tokio::spawn(self.process_writer(id, send));
    }

    fn process_reader(
        &self,
        id: ProcessUniqueId,
        reader: impl TryStream<Ok = BytesMut, Error = io::Error>,
    ) -> impl Future<Output = ()> {
        let shared = self.shared.clone();

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
            .forward(self.shared.sink.clone().sink_map_err(|e| {
                println!("receive channel error: {:?}", e);
            }))
            .then(move |_| {
                println!("{} disconnected", id);
                let mut clients = shared.clients.write().unwrap();
                clients.remove(&id);
                future::ready(())
            })
    }

    fn process_writer(
        &self,
        id: ProcessUniqueId,
        writer: impl Sink<Bytes, Error = impl Debug>,
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
            chan.unbounded_send(enc.clone().into()).unwrap();
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
