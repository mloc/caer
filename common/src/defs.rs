pub type ObjID = u32;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Appearance {
    pub alpha: i16,
    pub appearance_flags: i16,
    pub blend_mode: i16,
    pub color: Color,
    pub desc: String,
    pub gender: Gender,
    pub icon: i32,
    pub icon_state: String,
    pub infra_luminosity: i16,
    pub invisibility: i16,
    pub layer: f32,
    pub luminosity: i16,
    pub maptext: String,
    pub maptext_width: i16,
    pub maptext_height: i16,
    pub maptext_x: i16,
    pub maptext_y: i16,
    pub name: String,
    pub opacity: bool,
    pub pixel_x: i16,
    pub pixel_y: i16,
    pub pixel_w: i16,
    pub pixel_z: i16,
    pub plane: i16,
    pub transform: Vec<f32>,

    pub overlays: Vec<Appearance>,
    pub underlays: Vec<Appearance>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Color {
    White,
    RGB(u8, u8, u8),
    Matrix(Vec<f32>),
}

impl Default for Color {
    fn default() -> Color { Color::White }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Location {
    Null,
    Within(ObjID),
    Coords(i16, i16, i16),
}

impl Default for Location {
    fn default() -> Location { Location::Null }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Gender {
    Neuter,
    Male,
    Female,
    Plural,
}

impl Default for Gender {
    fn default() -> Gender { Gender::Neuter }
}
