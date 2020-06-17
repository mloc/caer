use crate::ty;

use caer_runtime;

use ena::unify::{self, InPlace, UnificationTable, UnifyKey, UnifyValue};
use index_vec::{IndexVec, Idx};
use thiserror::Error;

use std::collections::BTreeSet;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferKey(u32);

impl UnifyKey for InferKey {
    type Value = Option<InferValue>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "InferKey"
    }
}

impl Idx for InferKey {
    fn from_usize(idx: usize) -> Self {
        Self(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Default)]
pub struct InferValue {
    tys: BTreeSet<ty::Complex>,
    frozen: bool,
}

impl InferValue {
    fn from_ty(ty: ty::Complex) -> Self {
        Self {
            tys: std::iter::once(ty).collect(),
            frozen: false,
        }
    }

    pub fn as_ty(self) -> ty::Complex {
        if self.tys.is_empty() {
            ty::Complex::Any
        } else if let Some(prim) = self.as_primitive() {
            prim.into()
        } else {
            ty::Complex::OneOf(self.tys.clone())
        }
    }

    pub fn as_primitive(&self) -> Option<ty::Primitive> {
        if self.tys.len() != 1 {
            return None;
        }
        match self.tys.iter().next() {
            Some(ty::Complex::Primitive(prim)) => Some(*prim),
            _ => None,
        }
    }

    fn freeze(&self) -> Self {
        Self {
            tys: self.tys.clone(),
            frozen: true,
        }
    }
}

impl UnifyValue for InferValue {
    type Error = InferUnifyError;

    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        if v1.frozen && v1.tys != v2.tys {
            Err(InferUnifyError::UnifyFrozen {
                frozen: v1.tys.clone(),
                target: v2.tys.clone(),
            })
        } else if v2.frozen && v1.tys != v2.tys {
            Err(InferUnifyError::UnifyFrozen {
                frozen: v2.tys.clone(),
                target: v1.tys.clone(),
            })
        } else {
            Ok(Self {
                tys: v1.tys.union(&v2.tys).cloned().collect(),
                frozen: v1.frozen || v2.frozen,
            })
        }
    }
}

#[derive(Debug, Error)]
pub enum InferUnifyError {
    #[error("attempted to unify frozen ty set {frozen:?} with incompatible ty set {target:?}")]
    UnifyFrozen {
        frozen: BTreeSet<ty::Complex>,
        target: BTreeSet<ty::Complex>,
    },
    #[error("exhausted all branches on Choice step")]
    ExhaustedChoices,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Rule {
    Const(InferKey, ty::Complex),
    ConstFreeze(InferKey, ty::Complex),
    Equals(InferKey, InferKey),
    // TODO: break cr:op:Binaryop into cfg or something
    //BinOp(InferKey, caer_runtime::op::BinaryOp, InferKey, InferKey),
    // TODO break out proc ty from complex
    //Apply(ty::Complex, Vec<ty::TyId>, Option<ty::TyId>, Option<ty::TyId>),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Step {
    Single(Vec<Rule>),
    Choice(Vec<Vec<Rule>>),
}

pub struct InferEngine {
    table: UnificationTable<InPlace<InferKey>>,
}

impl InferEngine {
    pub fn new() -> Self {
        Self {
            table: UnificationTable::new(),
        }
    }

    pub fn add_var(&mut self) -> InferKey {
        self.table.new_key(None)
    }

    pub fn get_assignments(&mut self) -> IndexVec<InferKey, InferValue> {
        (0..self.table.len()).map(|i| {
            let key = InferKey::from_usize(i);
            self.table.probe_value(key).unwrap_or_default()
        }).collect()
    }

    pub fn probe_assignment(&mut self, key: InferKey) -> InferValue {
        self.table.probe_value(key).unwrap_or_default()
    }

    pub fn process_steps(&mut self, steps: &[Step], sub_rules: &[(InferKey, InferKey)]) -> Result<(), InferUnifyError> {
        match steps.split_first() {
            None => self.process_sub(sub_rules),
            Some((Step::Single(rules), rest)) => {
                self.process_rules(rules)?;
                self.process_steps(rest, sub_rules)
            }
            Some((Step::Choice(choices), rest)) => {
                let mut pre_snapshot = self.table.snapshot();
                for rules_choice in choices {
                    match self.process_rules(rules_choice).and_then(|_| self.process_steps(rest, sub_rules)) {
                        Ok(_) => {
                            self.table.commit(pre_snapshot);
                            return Ok(())
                        }
                        Err(_) => {
                            self.table.rollback_to(pre_snapshot);
                            pre_snapshot = self.table.snapshot();
                        }
                    }
                }
                self.table.rollback_to(pre_snapshot);
                Err(InferUnifyError::ExhaustedChoices)
            }
        }
    }

    pub fn process_rules(&mut self, rules: &[Rule]) -> Result<(), InferUnifyError> {
        for rule in rules {
            self.process_rule(rule)?
        }
        Ok(())
    }

    pub fn process_rule(&mut self, rule: &Rule) -> Result<(), InferUnifyError> {
        match rule {
            Rule::Const(key, ty) => {
                let const_val = Some(InferValue::from_ty(ty.clone()));
                self.table.unify_var_value(*key, const_val)?;
            }
            Rule::ConstFreeze(key, ty) => {
                let const_val = Some(InferValue::from_ty(ty.clone()).freeze());
                self.table.unify_var_value(*key, const_val)?;
            }
            Rule::Equals(k1, k2) => {
                self.table.unify_var_var(*k1, *k2)?;
            }
        }
        Ok(())
    }

    pub fn process_sub(&mut self, sub_rules: &[(InferKey, InferKey)]) -> Result<(), InferUnifyError> {
        for (lesser_key, greater_key) in sub_rules {
            let lesser_val = self.table.probe_value(*lesser_key);
            self.table.unify_var_value(*greater_key, lesser_val)?;
        }
        Ok(())
    }
}
