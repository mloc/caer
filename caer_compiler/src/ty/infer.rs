use crate::ty;

use caer_runtime;

use ena::unify::{self, InPlace, UnificationTable, UnifyKey, UnifyValue};
use index_vec::{IndexVec, Idx};
use thiserror::Error;
use petgraph::graph::DiGraph;

use std::collections::{BTreeSet, HashSet, HashMap};

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
    sub_constraints: Vec<(InferKey, InferKey)>,
}

impl InferEngine {
    pub fn new() -> Self {
        Self {
            table: UnificationTable::new(),
            sub_constraints: Vec::new(),
        }
    }

    pub fn add_var(&mut self) -> InferKey {
        self.table.new_key(None)
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

    pub fn process_subs(&mut self) -> Result<(), InferUnifyError> {
        let tab = &mut self.table;
        self.sub_constraints.retain(|(sub, sup)| !tab.unioned(*sub, *sup));
        // normalize keys to their root, for easier eq
        self.sub_constraints.iter_mut().for_each(|(sub, sup)| {
            *sub = tab.find(*sub);
            *sup = tab.find(*sup);
        });
        Ok(())
    }

    pub fn process_sub(&mut self, sub_rules: &[(InferKey, InferKey)]) -> Result<(), InferUnifyError> {
        self.sub_constraints = sub_rules.iter().cloned().collect();
        //for (lesser_key, greater_key) in sub_rules {
            //let lesser_val = self.table.probe_value(*lesser_key);
            //self.table.unify_var_value(*greater_key, lesser_val)?;
        //}
        Ok(())
    }

    pub fn resolve_assignments(&mut self) -> IndexVec<InferKey, InferValue> {
        let resolver = InferResolver::new(&mut self.table, self.sub_constraints.clone());
        resolver.resolve().unwrap()
    }
}

struct InferResolver<'i> {
    table: &'i mut UnificationTable<InPlace<InferKey>>,
    sub_constraints: Vec<(InferKey, InferKey)>,
}

impl<'i> InferResolver<'i> {
    fn new(table: &'i mut UnificationTable<InPlace<InferKey>>, subs: Vec<(InferKey, InferKey)>) -> Self {
        Self {
            table: table,
            sub_constraints: subs,
        }
    }

    fn resolve(mut self) -> Result<IndexVec<InferKey, InferValue>, InferUnifyError> {
        let snap = self.table.snapshot();
        let res = self.do_resolve();
        self.table.rollback_to(snap);
        res
    }

    fn do_resolve(&mut self) -> Result<IndexVec<InferKey, InferValue>, InferUnifyError> {
        self.tidy_constraints();
        self.eliminate_cycles()?;
        self.tidy_constraints();
        self.propogate_subtypes()?;

        Ok((0..self.table.len()).map(|i| {
            let key = InferKey::from_usize(i);
            self.table.probe_value(key).unwrap_or_default()
        }).collect())
    }

    fn get_graph(&self) -> DiGraph<InferKey, ()> {
        let mut graph = DiGraph::with_capacity(0, self.sub_constraints.len());

        let mut node_for_infer = HashMap::new();
        let mut get_node = |g: &mut DiGraph<InferKey, ()>, idx: InferKey| {
            *node_for_infer.entry(idx).or_insert_with(|| {
                g.add_node(idx)
            })
        };

        for (sub, sup) in self.sub_constraints.iter() {
            let sub_node = get_node(&mut graph, *sub);
            let sup_node = get_node(&mut graph, *sup);
            graph.add_edge(sub_node, sup_node, ());
        }

        graph
    }

    fn tidy_constraints(&mut self) {
        let c_tab = &mut self.table;
        self.sub_constraints.retain(|(sub, sup)| !c_tab.unioned(*sub, *sup));
        // normalize keys to their root, for easier eq
        self.sub_constraints.iter_mut().for_each(|(sub, sup)| {
            *sub = c_tab.find(*sub);
            *sup = c_tab.find(*sup);
        });
        println!("TIDY: {:?}", self.sub_constraints);
    }

    fn eliminate_cycles(&mut self) -> Result<(), InferUnifyError> {
        let graph = self.get_graph();

        let sccs = petgraph::algo::kosaraju_scc(&graph);
        for scc in sccs {
            scc.iter().try_fold(None, |prev, cur_idx| -> Result<Option<InferKey>, InferUnifyError> {
                let cur_key = graph[*cur_idx];
                match prev {
                    None => Ok(Some(cur_key)),
                    Some(prev_key) => {
                        self.table.unify_var_var(prev_key, cur_key)?;
                        Ok(Some(cur_key))
                    },
                }
            })?;
        }

        Ok(())
    }

    fn propogate_subtypes(&mut self) -> Result<(), InferUnifyError> {
        let graph = self.get_graph();

        let topsort = petgraph::algo::toposort(&graph, None).expect("subtype graph has cycles");

        for node in topsort {
            let sub_val = self.table.probe_value(graph[node]);
            for super_node in graph.neighbors_directed(node, petgraph::Direction::Outgoing) {
                let sub_val = self.table.probe_value(graph[node]);
                self.table.unify_var_value(graph[super_node], sub_val)?;
            }
        }

        Ok(())
    }
}
