use aed_server::server;

pub struct SyncServer {
    server: server::Server,
    stream: server::ServerStream<(server::ClientId, aed_common::messages::Client)>,
}

impl SyncServer {
    pub fn setup() -> Self {
        let (server, stream) = server::Server::start(&"0.0.0.0:2939".parse().unwrap());
        Self {
            server,
            stream,
        }
    }

    pub fn iter_messages(&mut self) -> impl Iterator<Item = (server::ClientId, aed_common::messages::Client)> + '_ {
        self.stream.iter()
    }
}
