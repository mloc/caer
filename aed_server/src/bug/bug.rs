extern crate bytes;
extern crate futures;
extern crate tokio;
extern crate tokio_io;

use tokio::net::TcpListener;
use tokio::prelude::*;
use tokio_io::codec::length_delimited;
use bytes::BytesMut;
use futures::sync::mpsc;

fn main() {
    let addr = "127.0.0.1:2978".parse().unwrap();
    let listener = TcpListener::bind(&addr).unwrap();

    let server = listener
        .incoming()
        .for_each(|sock| {
            println!("new client");

            let (recv, send) = sock.split();
            let reader = length_delimited::FramedRead::new(recv);
            let writer = length_delimited::FramedWrite::new(send);

            let (sq_w, sq_r) = mpsc::unbounded();

            tokio::spawn(
                reader
                    .map_err(|e| {
                        println!("recv error: {:?}", e);
                    })
                    .for_each(|v| {
                        println!("got msg");
                        Ok(())
                    })
            );

            tokio::spawn(
                sq_r
                    .map_err(|e| {
                        println!("send channel error: {:?}", e);
                    })
                    .forward(writer.sink_map_err(|e| {
                        println!("send error: {:?}", e);
                    }))
                    .then(|_| Ok(()))
            );

            let msg = BytesMut::from("hello");
            for _ in 0..10 {
                match sq_w.unbounded_send(msg.clone()) {
                    Ok(()) => {},
                    Err(_) => break,
                }
            }

            Ok(())
        })
        .map_err(|e| {
            println!("accept error: {:?}", e);
        });

    tokio::run(server);
}
