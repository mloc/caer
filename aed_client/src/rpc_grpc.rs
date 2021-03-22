// This file is generated. Do not edit
// @generated

// https://github.com/Manishearth/rust-clippy/issues/702
#![allow(unknown_lints)]
#![allow(clippy)]

#![cfg_attr(rustfmt, rustfmt_skip)]

#![allow(box_pointers)]
#![allow(dead_code)]
#![allow(missing_docs)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(trivial_casts)]
#![allow(unsafe_code)]
#![allow(unused_imports)]
#![allow(unused_results)]


// interface

pub trait Diby {
    fn serve(&self, o: ::grpc::RequestOptions, p: ::grpc::StreamingRequest<super::rpc::ClientMsg>) -> ::grpc::StreamingResponse<super::rpc::ServerMsg>;
}

// client

pub struct DibyClient {
    grpc_client: ::grpc::Client,
    method_Serve: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::rpc::ClientMsg, super::rpc::ServerMsg>>,
}

impl DibyClient {
    pub fn with_client(grpc_client: ::grpc::Client) -> Self {
        DibyClient {
            grpc_client: grpc_client,
            method_Serve: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/Diby/Serve".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Bidi,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
        }
    }

    pub fn new_plain(host: &str, port: u16, conf: ::grpc::ClientConf) -> ::grpc::Result<Self> {
        ::grpc::Client::new_plain(host, port, conf).map(|c| {
            DibyClient::with_client(c)
        })
    }
    pub fn new_tls<C : ::tls_api::TlsConnector>(host: &str, port: u16, conf: ::grpc::ClientConf) -> ::grpc::Result<Self> {
        ::grpc::Client::new_tls::<C>(host, port, conf).map(|c| {
            DibyClient::with_client(c)
        })
    }
}

impl Diby for DibyClient {
    fn serve(&self, o: ::grpc::RequestOptions, p: ::grpc::StreamingRequest<super::rpc::ClientMsg>) -> ::grpc::StreamingResponse<super::rpc::ServerMsg> {
        self.grpc_client.call_bidi(o, p, self.method_Serve.clone())
    }
}

// server

pub struct DibyServer;


impl DibyServer {
    pub fn new_service_def<H : Diby + 'static + Sync + Send + 'static>(handler: H) -> ::grpc::rt::ServerServiceDefinition {
        let handler_arc = ::std::sync::Arc::new(handler);
        ::grpc::rt::ServerServiceDefinition::new("/Diby",
            vec![
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/Diby/Serve".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Bidi,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerBidi::new(move |o, p| handler_copy.serve(o, p))
                    },
                ),
            ],
        )
    }
}
