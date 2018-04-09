#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Client {
    Message(String)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Server {
    Message(String)
}
