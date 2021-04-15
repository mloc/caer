use crate::defs::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Client {
    Message(String),

    VerbCall(VerbCall),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Server {
    Message(String),

    NewObject(ObjectState),
    UpdateObject(ObjectState),
    DelObject(ObjId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorldState {
    pub bounds: (i16, i16, i16),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectState {
    pub id: ObjId,
    pub loc: Location,
    pub appearance: Appearance,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerbCall {
    pub verb_id: VerbId,
    pub usr: UserId,
    pub src: ObjId,
    // TODO: yeah
    pub args: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserOutput {
    pub user: UserId,
    // TODO: other output types
    pub text: String,
}
