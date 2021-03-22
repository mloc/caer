use crate::defs::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Client {
    Message(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Server {
    Message(String),

    NewObject(ObjectState),
    UpdateObject(ObjectState),
    DelObject(ObjID),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorldState {
    pub bounds: (i16, i16, i16),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectState {
    pub id: ObjID,
    pub loc: Location,
    pub appearance: Appearance,
}
