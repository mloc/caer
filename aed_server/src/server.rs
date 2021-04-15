use futures::channel::mpsc;
use futures::Future;
use futures::future;
use futures::TryStream;
use futures::SinkExt;
use futures::FutureExt;
use std::fmt::Debug;
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
use std::pin::Pin;
use futures::task::Poll;

// Bit of a hack while aed and caer are separate
pub struct ServerStream<I> {
    stream: Pin<Box<dyn Stream<Item = I>>>,
}

impl<I> ServerStream<I> {
    pub fn poll(&mut self) -> Option<I> {
        let waker = futures::task::noop_waker_ref();
        let mut cx = std::task::Context::from_waker(waker);

        let poll = self.stream.poll_next_unpin(&mut cx); // TODO errors
        match poll {
            Poll::Ready(t) => t,
            Poll::Pending => None,
        }
    }

    pub fn iter(&mut self) -> impl Iterator<Item = I> + '_ {
        std::iter::from_fn(move || self.poll())
    }

    pub fn decompose(self) -> Pin<Box<dyn Stream<Item = I>>> {
        self.stream
    }
}


#[derive(Debug, Clone)]
pub struct Server {
    shared: Arc<ServerShared>,
}

pub type ClientId = ProcessUniqueId;

#[derive(Debug)]
struct ServerShared {
    runtime: Arc<tokio::runtime::Runtime>,
    sink: mpsc::UnboundedSender<(ClientId, messages::Client)>,

    pub clients: RwLock<IndexMap<ProcessUniqueId, mpsc::UnboundedSender<Bytes>>>,
    addrs: RwLock<IndexMap<ProcessUniqueId, SocketAddr>>,
}

impl Server {
    pub fn start(
        addr: &SocketAddr,
    ) -> (
        Self,
        ServerStream<(ProcessUniqueId, messages::Client)>,
    ) {
        let rt = Arc::new(tokio::runtime::Runtime::new().unwrap());
        let (sink, source) = mpsc::unbounded();

        let server = Server {
            shared: Arc::new(ServerShared {
                runtime: rt.clone(),
                sink,

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

        (server, ServerStream { stream: Box::pin(source) })
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
                {
                    let mut clients = shared.clients.write().unwrap();
                    clients.remove(&id);
                }
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
