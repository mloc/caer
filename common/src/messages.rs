use defs::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Client {
    Message(String)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Server {
    Message(String)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectState {
    pub id: u32,
    pub loc: Option<Location>,
    pub appearance: Option<Appearance>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Appearance {
    pub entries: Vec<AppearanceEntry>,
    pub overlays: Vec<Appearance>,
    pub underlays: Vec<Appearance>,
}
