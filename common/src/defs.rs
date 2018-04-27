#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AppearanceEntry {
    Alpha(i16),
    AppearanceFlags(i16),
    BlendMode(i16),
    Color(Color),
    Desc(String),
    Gender(Gender),
    Icon(String),
    IconState(String),
    Invisibility(i16),
    InfraLuminosity(i16),
    Layer(f32),
    Luminosity(i16),
    Maptext(String),
    MaptextWidth(i16),
    MaptextHeight(i16),
    MaptextX(i16),
    MaptextY(i16),
    Name(String),
    Opacity(bool),
    PixelX(i16),
    PixelY(i16),
    PixelW(i16),
    PixelZ(i16),
    Plane(i16),
    Transform(Vec<f32>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Color {
    White,
    RGB(u8, u8, u8),
    Matrix(Vec<f32>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Location {
    Null,
    Within(u32),
    Coords(i16, i16, i16),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Gender {
    Neuter,
    Male,
    Female,
    Plural,
}
