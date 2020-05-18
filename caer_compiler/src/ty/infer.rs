use crate::ty;

pub enum Rule {
    Equals(ty::TyId, ty::TyId),
    // TODO break out proc ty from complex
    Apply(ty::Complex, Vec<ty::TyId>, Option<ty::TyId>, Option<ty::TyId>),
}
