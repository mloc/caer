extern crate bytes;
extern crate common;
extern crate futures;
extern crate serde_cbor;
extern crate tokio;
extern crate tokio_io;
extern crate tokio_serde_cbor;
extern crate tokio_serde_json;

use common::messages;
use tokio::net::TcpListener;
use tokio::prelude::*;
use tokio_io::codec::length_delimited;
use bytes::BytesMut;
use futures::sync::mpsc;

struct Server {}

struct Connection<R: AsyncRead, W: AsyncWrite> {
    reader: length_delimited::FramedRead<R>,
    writer: length_delimited::FramedWrite<W, Vec<u8>>,
}

/*impl<R: AsyncRead, W: AsyncWrite> Connection<R, W> {
    fn send(&self, msg: messages::Server) -> futures::sink::Send<length_delimited::FramedWrite<W>> {
        se
    }
}*/

fn main() {
    let addr = "127.0.0.1:2978".parse().unwrap();
    let listener = TcpListener::bind(&addr).unwrap();

    let server = listener
        .incoming()
        .for_each(|sock| {
            println!("hallo");
            let (recv, send) = sock.split();
            let conn = Connection {
                reader: length_delimited::FramedRead::new(recv),
                writer: length_delimited::FramedWrite::new(send),
            };

            let (rq_w, rq_r) = mpsc::unbounded();
            let (sq_w, sq_r) = mpsc::unbounded();

            tokio::spawn(
                conn.reader
                    .map(|x| serde_cbor::de::from_slice::<common::messages::Client>(&x))
                    .map_err(|e| {
                        println!("recv error: {:?}", e);
                    })
                    .forward(rq_w.sink_map_err(|e| {
                        println!("recv channel error: {:?}", e);
                    }))
                    .then(|_| Ok(())),
            );

            tokio::spawn(
                sq_r
                    .map_err(|e| {
                        println!("send channel error: {:?}", e);
                    })
                    .then(|x| serde_cbor::ser::to_vec(&x.unwrap()))
                    .map_err(|e| {
                        println!("encode error: {:?}", e);
                    })
                    .forward(conn.writer.sink_map_err(|e| {
                        println!("send error: {:?}", e);
                    }))
                    .then(|_| Ok(()))
            );

            let msg = messages::Server::Message("hallo".to_string());
            for _ in 0..1000000 {
                match sq_w.unbounded_send(msg.clone()) {
                    Ok(()) => {},
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

    tokio::run(server);
}
