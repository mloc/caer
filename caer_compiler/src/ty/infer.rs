use crate::ty;

use caer_runtime;

use ena::unify::{self, InPlace, UnificationTable, UnifyKey, UnifyValue};
use index_vec::{IndexVec, Idx};
use thiserror::Error;

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

#[derive(Debug, Clone)]
pub struct InferValue {
    ty: ty::Complex,
    frozen: bool,
}

impl InferValue {
    fn from_ty(ty: ty::Complex) -> Self {
        Self {
            ty: ty,
            frozen: false,
        }
    }

    fn freeze(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            frozen: true,
        }
    }
}

impl UnifyValue for InferValue {
    type Error = InferUnifyError;

    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        if v1.frozen && v1.ty != v2.ty {
            Err(InferUnifyError::UnifyFrozen {
                frozen: v1.ty.clone(),
                target: v2.ty.clone(),
            })
        } else if v2.frozen && v1.ty != v2.ty {
            Err(InferUnifyError::UnifyFrozen {
                frozen: v2.ty.clone(),
                target: v1.ty.clone(),
            })
        } else {
            Ok(Self {
                ty: ty::Complex::unify(vec![&v1.ty, &v2.ty].into_iter()),
                frozen: v1.frozen || v2.frozen,
            })
        }
    }
}

#[derive(Debug, Error)]
pub enum InferUnifyError {
    #[error("attempted to unify frozen ty {frozen:?} with incompatible ty {target:?}")]
    UnifyFrozen {
        frozen: ty::Complex,
        target: ty::Complex,
    },
    #[error("exhausted all branches on Choice step")]
    ExhaustedChoices,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Rule {
    Const(InferKey, ty::Complex),
    ConstFreeze(InferKey, ty::Complex),
    // more a subtyping relationship, builds union
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

    pub fn get_assignments(&mut self) -> IndexVec<InferKey, Option<InferValue>> {
        (0..self.table.len()).map(|i| {
            let key = InferKey::from_usize(i);
            self.table.probe_value(key)
        }).collect()
    }

    pub fn process_steps(&mut self, steps: &[Step]) -> Result<(), InferUnifyError> {
        match steps.split_first() {
            None => Ok(()),
            Some((Step::Single(rules), rest)) => {
                self.process_rules(rules)?;
                self.process_steps(rest)
            }
            Some((Step::Choice(choices), rest)) => {
                let mut pre_snapshot = self.table.snapshot();
                for rules_choice in choices {
                    match self.process_rules(rules_choice).and_then(|_| self.process_steps(rest)) {
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

    fn process_rules(&mut self, rules: &[Rule]) -> Result<(), InferUnifyError> {
        for rule in rules {
            self.process_rule(rule)?
        }
        Ok(())
    }

    fn process_rule(&mut self, rule: &Rule) -> Result<(), InferUnifyError> {
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
}
