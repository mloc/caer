mod server;
mod master_controller;
mod runtime;

use aed_common::{messages, defs};
use tokio::net::TcpListener;
use tokio_stream::StreamExt;
use tokio_util::codec::length_delimited;
use tokio::io::{AsyncRead, AsyncWrite};
use bytes::BytesMut;
use tokio::sync::mpsc;
use std::sync::Arc;

/*pub struct Server {}

#[expose_c_stubs(test)]
impl Server {
    fn new(name: &u32, anp: u32) -> Self {
        Server {}
    }
    fn new2(&mut self, x: u32) -> Self {
        Server {}
    }
    fn wah(&mut self) -> u32 {
        3
    }
}*/

/*struct Connection<R: AsyncRead, W: AsyncWrite> {
    reader: length_delimited::FramedRead<R>,
    writer: length_delimited::FramedWrite<W, Vec<u8>>,
}*/

#[tokio::main]
async fn main() {
    let (srv, mut src) = server::Server::start(&"0.0.0.0:2939".parse().unwrap()).await;

    let proc_srv = srv.clone();
    while let Some((id, msg)) = src.next().await {
        println!("{}: {:?}", id, msg);
        match msg {
            messages::Client::Message(s) => {
                println!("{}", s);
                let appearance = defs::Appearance::default();

                let os = messages::ObjectState{
                    id: 10,
                    loc: defs::Location::Coords(1,2,3),
                    appearance,
                };
                proc_srv.send(id, &messages::Server::UpdateObject(os));
            },
        };
    }

    /*let addr = "127.0.0.1:2978".parse().unwrap();
    let listener = TcpListener::bind(&addr).unwrap();

    let (rq_w, rq_r) = mpsc::unbounded();

    let inq = Arc::new(rq_w.clone());
    let server = listener
        .incoming()
        .for_each(move |sock| {
            let inq = (*inq).clone();
            println!("hallo");
            let (recv, send) = sock.split();
            let conn = Connection {
                reader: length_delimited::FramedRead::new(recv),
                writer: length_delimited::FramedWrite::new(send),
            };

            let (sq_w, sq_r) = mpsc::unbounded();

            tokio::spawn(
                conn.reader
                    .map_err(|e| {
                        println!("recv error: {:?}", e);
                    })
                    .and_then(|x| {
                        serde_cbor::de::from_slice::<common::messages::Client>(&x).map_err(|e| {
                            println!("decode error: {:?}", e);
                        })
                    })
                    .forward(inq.sink_map_err(|e| {
                        println!("recv channel error: {:?}", e);
                    }))
                    .then(|_| Ok(())),
            );

            tokio::spawn(
                sq_r.map_err(|e| {
                    println!("send channel error: {:?}", e);
                })
                .and_then(|x| {
                    serde_cbor::ser::to_vec(&x).map_err(|e| {
                        println!("encode error: {:?}", e);
                    })
                })
                .forward(conn.writer.sink_map_err(|e| {
                    println!("send error: {:?}", e);
                }))
                .then(|_| Ok(())),
            );

            let msg = messages::Server::Message("hallo".to_string());
            for _ in 0..1 {
                match sq_w.unbounded_send(msg.clone()) {
                    Ok(()) => {}
                    Err(_) => break,
                }
                // tokio::spawn(sq_w.clone().send(BytesMut::from("hallo")).then(|_| Ok(())));
            }

            /*let act = conn.writer.send(BytesMut::from("abc"));
        let act = act.and_then(|w| { w.send(BytesMut::from("abc")) }).then(|_| { Ok(()) });
        tokio::spawn(act);*/

            Ok(())
        })
        .map_err(|e| {
            println!("accept error: {:?}", e);
        });

    tokio::run(fut);*/
}
