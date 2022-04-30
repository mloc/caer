use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{Seek, SeekFrom, Write};

use caer_types::func::{CallingSpec, ClosureSpec, FuncInfo};
use caer_types::id::{FuncId, PathTypeId, StringId, TypeId, TYPE_ID_ANY};
use caer_types::ty::{self, RefType};
use index_vec::IndexVec;
use serde::Serialize;
use ty::Type;

use super::id::*;
use crate::module::Module;

#[derive(Debug, Clone, Serialize)]
pub struct Local {
    pub id: LocalId,
    pub ty: TypeId,
    pub movable: bool,
    pub construct_scope: ScopeId,
    // if a value is moved, it won't be destructed with this local
    pub destruct_scope: Option<ScopeId>,
    // DM-style "compile-time" typepath
    pub assoc_dty: Option<PathTypeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Var {
    pub id: VarId,
    pub name: StringId,
    pub scope: ScopeId,
    pub ty: TypeId,
    // DM-style "compile-time" typepath, only used for error checking safe deref ops
    // TODO: maybe fold into ty?
    pub assoc_dty: Option<PathTypeId>,

    pub captures: HashMap<FuncId, VarId>,
}

#[derive(Debug)]
pub struct LocalFlow {
    pub id: LocalId,

    pub reads: i32,
    pub takes: i32,
}

impl LocalFlow {
    fn new(local: &Local) -> Self {
        Self {
            id: local.id,

            reads: 0,
            takes: 0,
        }
    }
}

// TODO: move, along with proc. this is more meta than cfg
#[derive(Debug, Clone, Serialize)]
pub struct Closure {
    pub over: FuncId,
    // scope in the parent
    pub scope: ScopeId,
    // maps a var in THIS func to the var in the PARENT func that's captured
    pub captured: Vec<(VarId, VarId)>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Function {
    pub id: FuncId,
    // For method procs: the func called by ..()
    // TODO: should this be moved into some proc bookkeeping?
    pub parent: Option<FuncId>,

    pub blocks: IndexVec<BlockId, Block>,
    pub scopes: IndexVec<ScopeId, Scope>,
    pub global_scope: ScopeId,

    pub locals: IndexVec<LocalId, Local>,
    pub vars: IndexVec<VarId, Var>,
    pub vars_by_name: HashMap<StringId, VarId>,

    pub params: Vec<VarId>,
    pub closure: Option<Closure>,
    pub child_closures: IndexVec<ClosureSlotId, FuncId>,

    pub calling_spec: Option<CallingSpec>,

    // Hacky. Used for shim gen.
    pub builtin: bool,
}

impl<'a> Function {
    pub fn new(id: FuncId) -> Self {
        // ids map to indices in the scopes list; we can assume this scope will have id 0
        // TODO sounder way to handle this + locals?
        let mut scopes = IndexVec::new();
        let global_scope = scopes.next_idx();
        scopes.push(Scope::new(global_scope, None));

        let mut new = Self {
            id,
            parent: None,

            blocks: IndexVec::new(),
            scopes,
            global_scope,

            locals: IndexVec::new(),
            vars: IndexVec::new(),
            vars_by_name: HashMap::new(),

            params: Vec::new(),
            closure: None,
            child_closures: IndexVec::new(),

            calling_spec: None,

            builtin: false,
        };

        // TODO: compile time intern, "."
        new.add_var(new.global_scope, TYPE_ID_ANY, StringId::new(0)); // return var
        new
    }

    pub fn set_closure(&mut self, over: FuncId, over_scope: ScopeId) {
        assert!(
            self.closure.is_none(),
            "proc is already marked as a closure, {:?}",
            self.closure
        );
        self.closure = Some(Closure {
            over,
            scope: over_scope,
            captured: Vec::new(),
        });
    }

    pub fn add_local(&mut self, scope: ScopeId, ty: TypeId) -> LocalId {
        let id = self.locals.next_idx();

        let local = Local {
            id,
            ty,
            movable: false,
            construct_scope: scope,
            destruct_scope: Some(scope),
            assoc_dty: None,
        };

        self.locals.push(local);
        self.scopes[scope].locals.push(id);
        self.scopes[scope].destruct_locals.insert(id);

        id
    }

    // TODO: localref
    pub fn set_assoc_dty(&mut self, local: LocalId, dty: PathTypeId) {
        self.locals[local].assoc_dty = Some(dty);
    }

    pub fn get_assoc_dty(&self, local: LocalId) -> Option<PathTypeId> {
        self.locals[local].assoc_dty
    }

    // TODO: ERRH(C), duplicate vars
    pub fn add_var(&mut self, scope: ScopeId, ty: TypeId, name: StringId) -> VarId {
        let id = self.vars.next_idx();
        let var_info = Var {
            id,
            scope,
            name,
            ty,
            assoc_dty: None,
            captures: HashMap::new(),
        };

        self.vars.push(var_info);
        self.scopes[scope].vars.push(id);
        self.scopes[scope].vars_by_name.insert(name, id);

        id
    }

    pub fn add_captured_var(&mut self, name: StringId, to_cap: VarId) -> VarId {
        // captured vars exist at the global scope, are always soft (for now)
        let id = self.add_var(self.global_scope, TYPE_ID_ANY, name);

        match &mut self.closure {
            None => panic!("can only capture variables if proc is marked as a closure"),
            Some(closure) => closure.captured.push((id, to_cap)),
        }

        id
    }

    pub fn mark_var_captured(&mut self, my_var: VarId, closure: FuncId, closure_var: VarId) {
        self.vars[my_var].captures.insert(closure, closure_var);
    }

    pub fn add_block(&mut self, block: Block) {
        assert_eq!(
            block.id,
            self.blocks.next_idx(),
            "must add blocks in sequential ID order; got {:?} but expected {:?}",
            block.id,
            self.blocks.next_idx()
        );
        self.scopes[block.scope].blocks.push(block.id);
        self.blocks.push(block);
    }

    // scope tree search to find matching var
    pub fn lookup_var(&self, root_scope: ScopeId, var: StringId) -> Option<VarId> {
        let mut cur_scope = root_scope;
        loop {
            let scope = &self.scopes[cur_scope];
            println!("{:?}", scope.vars_by_name);
            match scope.vars_by_name.get(&var) {
                Some(local) => return Some(*local),
                None => match scope.parent {
                    Some(parent) => cur_scope = parent,
                    None => break,
                },
            }
        }

        None
    }

    pub fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        let id = self.scopes.next_idx();
        self.scopes[parent].children.push(id);
        let scope = Scope::new(id, Some((parent, self.scopes[parent].depth)));
        self.scopes.push(scope);
        id
    }

    // analysis procs
    // TODO split up, move out of Proc maybe, and track as we build
    // TODO a smarter analysis method; this works but misses a lot
    pub fn analyze(&mut self) {
        let mut flow: IndexVec<_, _> = self.locals.iter().map(|l| LocalFlow::new(l)).collect();

        for block in self.blocks.iter() {
            for op in block.ops.iter() {
                match op {
                    Op::Noop => {},

                    Op::Literal(_, _) => {
                        // hmmm
                    },

                    Op::MkVar(_) => {},

                    Op::Load(_, _) => {},

                    Op::Store(_, _) => {},

                    Op::Put(src) => {
                        flow[*src].reads += 1;
                    },

                    Op::Binary(_, _, lhs, rhs) => {
                        flow[*lhs].reads += 1;
                        flow[*rhs].reads += 1;
                    },

                    Op::HardBinary(_, _, lhs, rhs) => {
                        flow[*lhs].reads += 1;
                        flow[*rhs].reads += 1;
                    },

                    Op::Call(_, _, args) => {
                        for arg in args.iter() {
                            flow[*arg].reads += 1;
                        }
                    },

                    Op::Cast(_, src, _) => {
                        flow[*src].reads += 1;
                    },

                    Op::AllocDatum(_, _) => {},

                    Op::DatumLoadVar(_, src, _) => {
                        flow[*src].reads += 1;
                    },

                    Op::DatumStoreVar(_, _, src) => {
                        flow[*src].reads += 1;
                    },

                    Op::DatumCallProc(_, dl, _, args) => {
                        flow[*dl].reads += 1;
                        for arg in args.iter() {
                            flow[*arg].reads += 1;
                        }
                    },

                    Op::Throw(src) => {
                        flow[*src].reads += 1;
                    },

                    Op::CatchException(_) => {},

                    Op::Spawn(_, Some(src)) => {
                        flow[*src].reads += 1;
                    },
                    Op::Spawn(_, None) => {},

                    Op::Sleep(src) => {
                        flow[*src].reads += 1;
                    },
                }
            }

            match block.terminator {
                Terminator::Switch {
                    discriminant,
                    branches: _,
                    default: _,
                } => {
                    flow[discriminant].reads += 1;
                },
                Terminator::Return | Terminator::Jump(_) | Terminator::TryCatch { .. } => {},
            }
        }

        // promote scopes
        for local_flow in flow.iter() {
            let local = &mut self.locals[local_flow.id];

            if local_flow.reads == 0 && local_flow.takes == 1 {
                // move case
                let old_scope = &mut self.scopes[local.destruct_scope.unwrap()];
                old_scope.destruct_locals.remove(&local_flow.id);

                local.movable = true;
                local.destruct_scope = None;
            }
        }
    }

    pub fn get_spec(&self) -> FuncInfo {
        let calling_spec;
        // TODO: fold closure var into calling spec?
        // -> calling spec cannot ref var ids rn though. func var ids could become part of the rt
        // info, though
        if let Some(closure) = &self.closure {
            calling_spec = CallingSpec::Closure(ClosureSpec {
                closure_parent: closure.over,
                env_size: closure.captured.len() as _,
            })
        } else {
            calling_spec = self
                .calling_spec
                .as_ref()
                .expect("func has no calling spec, but func spec was requested")
                .clone();
        }

        FuncInfo { calling_spec }
    }

    pub fn dot(&self, module: &Module, name: &str) {
        // TODO bad, for now we assume procs have unique names
        fs::create_dir_all("dbgout/dot/tino_cfg/").unwrap();
        fs::create_dir_all("dbgout/ir/").unwrap();
        // TODO: better string format for CFG
        serde_json::to_writer_pretty(
            File::create(format!("dbgout/ir/cfg_{}.json", name)).unwrap(),
            self,
        )
        .unwrap();
        // TODO: replace proc-name here with some better repr
        let mut f = File::create(format!("dbgout/dot/tino_cfg/cfg_{}.dot", name)).unwrap();
        dot::render(self, &mut f).unwrap();

        f.seek(SeekFrom::End(-2)).unwrap();
        //write!(f, "newrank=true;\n").unwrap();

        let finagle = |s: String| {
            s[1..s.len() - 1]
                .to_string()
                .replace("{", "\\{")
                .replace("}", "\\}")
        };

        let locals = self
            .locals
            .iter_enumerated()
            .map(|(id, l)| {
                finagle(
                    dot::LabelText::escaped(format!(
                        "{}: {:?}",
                        id.index(),
                        module.types.get(l.ty)
                    ))
                    .to_dot_string(),
                )
            })
            .collect::<Vec<_>>()
            .join("\\l");
        let vars = self
            .vars
            .iter_enumerated()
            .map(|(id, v)| {
                finagle(
                    dot::LabelText::escaped(format!(
                        "{}: {:?}",
                        id.index(),
                        module.types.get(v.ty)
                    ))
                    .to_dot_string(),
                )
            })
            .collect::<Vec<_>>()
            .join("\\l");
        let tys = module
            .types
            .iter()
            .map(|(id, ty)| {
                finagle(
                    dot::LabelText::escaped(format!("{}: {:?}", id.index(), ty)).to_dot_string(),
                )
            })
            .collect::<Vec<_>>()
            .join("\\l");
        writeln!(
            f,
            "llegend[label=\"{{Locals\\l|{{{}\\l}}}}\"][shape=\"record\"];",
            locals
        )
        .unwrap();
        writeln!(
            f,
            "vlegend[label=\"{{Vars\\l|{{{}\\l}}}}\"][shape=\"record\"];",
            vars
        )
        .unwrap();
        writeln!(
            f,
            "tlegend[label=\"{{Types (global)\\l|{{{}\\l}}}}\"][shape=\"record\"];",
            tys
        )
        .unwrap();

        self.dot_scope_cluster(&mut f, self.global_scope);
        writeln!(&mut f, "}}").unwrap();
    }

    fn dot_scope_cluster(&self, f: &mut File, scope_id: ScopeId) {
        let scope = &self.scopes[scope_id];
        let locals = scope
            .locals
            .iter()
            .map(|l| l.index().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(f, "subgraph cluster_scope{} {{", scope_id.index()).unwrap();
        writeln!(
            f,
            "label=\"scope {} [{}] LP: {:?}\";",
            scope.id.index(),
            locals,
            scope.landingpad
        )
        .unwrap();
        for block_id in scope.blocks.iter() {
            writeln!(f, "block{};", block_id.index()).unwrap();
        }
        for child in scope.children.iter() {
            self.dot_scope_cluster(f, *child);
        }
        writeln!(f, "graph[style=dotted];}}").unwrap();
    }
}

impl<'a> dot::Labeller<'a, BlockId, (BlockId, BlockId, String)> for Function {
    fn node_id(&'a self, n: &BlockId) -> dot::Id<'a> {
        dot::Id::new(format!("block{}", n.index())).unwrap()
    }

    fn node_shape(&'a self, _: &BlockId) -> Option<dot::LabelText<'a>> {
        Some(dot::LabelText::label("record"))
    }

    fn node_label(&'a self, n: &BlockId) -> dot::LabelText<'a> {
        let block = &self.blocks[*n];

        let term_str = match &block.terminator {
            Terminator::Return => "return".into(),
            Terminator::Jump(_) => "jump".into(),
            Terminator::Switch {
                discriminant,
                branches: _,
                default: _,
            } => format!("switch {:?}", discriminant),
            Terminator::TryCatch { .. } => "try/catch".into(),
        };

        let end_str = if block.scope_end { "END SCOPE\\l|" } else { "" };

        dot::LabelText::escaped(format!(
            "{{{}|{}\\l|{}{{{}}}}}",
            block.id.index(),
            block
                .ops
                .iter()
                .map(|op| format!("{:?}", op).replace("\\", "\\\\"))
                .collect::<Vec<_>>()
                .join("\\l"),
            end_str,
            term_str
        ))
    }

    fn edge_label(&'a self, e: &(BlockId, BlockId, String)) -> dot::LabelText<'a> {
        dot::LabelText::label(e.2.clone())
    }

    fn graph_id(&'a self) -> dot::Id<'a> {
        // TODO: extract actual name
        dot::Id::new(format!("block_{}", self.id.index().to_string())).unwrap()
    }
}

impl<'a> dot::GraphWalk<'a, BlockId, (BlockId, BlockId, String)> for Function {
    fn nodes(&self) -> dot::Nodes<'a, BlockId> {
        self.blocks.iter().map(|b| b.id).collect()
    }

    fn edges(&self) -> dot::Edges<'a, (BlockId, BlockId, String)> {
        self.blocks
            .iter()
            .flat_map(|block| match &block.terminator {
                Terminator::Return => vec![],
                Terminator::Jump(target) => vec![(block.id, *target, "".into())],
                Terminator::Switch {
                    discriminant: _,
                    branches,
                    default,
                } => {
                    let mut out = vec![(block.id, *default, "default".into())];

                    for (val, target) in branches.iter() {
                        out.push((block.id, *target, val.to_string()));
                    }

                    out
                },
                Terminator::TryCatch {
                    try_block,
                    catch_block,
                } => vec![
                    (block.id, *try_block, "try".into()),
                    (
                        block.id,
                        *catch_block,
                        format!("catch ({})", catch_block.index()),
                    ),
                ],
            })
            .collect::<Vec<_>>()
            .into()
    }

    fn source(&self, e: &(BlockId, BlockId, String)) -> BlockId {
        e.0
    }

    fn target(&self, e: &(BlockId, BlockId, String)) -> BlockId {
        e.1
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Block {
    pub id: BlockId,
    pub ops: Vec<Op>,
    pub terminator: Terminator,
    pub scope: ScopeId,
    pub scope_begin: bool,
    pub scope_end: bool,
}

impl Block {
    pub fn new(id: BlockId, scope: ScopeId) -> Self {
        Self {
            id,
            ops: Vec::new(),
            terminator: Terminator::Return,
            scope,
            scope_begin: false,
            scope_end: false,
        }
    }

    // uhh
    pub fn push_op(&mut self, op: Op) {
        self.ops.push(op)
    }

    // bad
    pub fn iter_successors(&self) -> Box<dyn Iterator<Item = BlockId>> {
        match &self.terminator {
            Terminator::Return => Box::new(std::iter::empty()),
            Terminator::Jump(id) => Box::new(std::iter::once(*id)),
            Terminator::Switch {
                discriminant: _,
                branches,
                default,
            } => Box::new(
                branches
                    .clone()
                    .into_iter()
                    .map(|(_, id)| id)
                    .chain(std::iter::once(*default)),
            ),
            Terminator::TryCatch {
                try_block,
                catch_block,
            } => Box::new(std::iter::once(*try_block).chain(std::iter::once(*catch_block))),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub depth: i32,
    pub locals: Vec<LocalId>,
    pub destruct_locals: HashSet<LocalId>,
    pub vars: Vec<VarId>,
    pub destruct_vars: HashSet<VarId>,
    pub vars_by_name: HashMap<StringId, VarId>,
    pub blocks: Vec<BlockId>,
    pub landingpad: Option<BlockId>,
}

impl Scope {
    fn new(id: ScopeId, parent: Option<(ScopeId, i32)>) -> Self {
        Self {
            id,
            parent: parent.map(|(id, _)| id),
            children: Vec::new(),
            depth: parent.map_or(0, |(_, d)| d + 1),
            locals: Vec::new(),
            destruct_locals: HashSet::new(),
            destruct_vars: HashSet::new(),
            vars: Vec::new(),
            vars_by_name: HashMap::new(),
            blocks: Vec::new(),
            landingpad: None,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum Op {
    Noop,

    Literal(LocalId, Literal),

    MkVar(VarId),
    Load(LocalId, VarId),
    Store(VarId, LocalId),

    Put(LocalId),
    // TODO: merge hard and soft, soft is a variant op with (Any, Any) -> Any
    Binary(LocalId, caer_types::op::BinaryOp, LocalId, LocalId),
    HardBinary(LocalId, caer_types::op::HardBinary, LocalId, LocalId),

    // TODO: STRINGID
    // TODO: use + ref a procid
    //Call(LocalId, StringId, Vec<LocalId>, Vec<(StringId, LocalId)>),
    Call(LocalId, StringId, Vec<LocalId>),

    Cast(LocalId, LocalId, TypeId),

    // TODO: move into an "RTOP" variant? or datum-op, idk
    // TODO: handle args to New()
    // TODO: handle prefabs: PathId? PrefabId?
    AllocDatum(LocalId, PathTypeId),
    DatumLoadVar(LocalId, LocalId, StringId), // local1 = local2.var
    DatumStoreVar(LocalId, StringId, LocalId), // local1.var = local2
    DatumCallProc(LocalId, LocalId, StringId, Vec<LocalId>), // local1 = local2.proc(args)

    Throw(LocalId),
    // analogous to llvm's landingpad instr
    // must be the first op in a catch block
    // specifies the var for the caught exception val, if provided
    // TODO: rework to be less llvm-bound
    CatchException(Option<VarId>),

    Spawn(ClosureSlotId, Option<LocalId>),
    Sleep(LocalId),
}

impl Op {
    pub fn dest_local(&self) -> Option<LocalId> {
        match self {
            Op::Noop => None,
            Op::Literal(dst, _) => Some(*dst),
            Op::MkVar(_) => None,
            Op::Load(dst, _) => Some(*dst),
            Op::Store(_, _) => None,
            Op::Put(_) => None,
            Op::Binary(dst, _, _, _) => Some(*dst),
            Op::HardBinary(dst, _, _, _) => Some(*dst),
            Op::Call(dst, _, _) => Some(*dst),
            Op::Cast(dst, _, _) => Some(*dst),
            Op::AllocDatum(dst, _) => Some(*dst),
            Op::DatumLoadVar(dst, _, _) => Some(*dst),
            Op::DatumStoreVar(_, _, _) => None,
            Op::DatumCallProc(dst, _, _, _) => Some(*dst),
            Op::Throw(_) => None,
            Op::CatchException(_) => None,
            Op::Spawn(_, _) => None,
            Op::Sleep(_) => None,
        }
    }

    // vec is meh, oh well. could do hacky iter?
    pub fn source_locals(&self) -> Vec<LocalId> {
        match self {
            Op::Noop => vec![],
            Op::Literal(_, _) => vec![],
            Op::MkVar(_) => vec![],
            Op::Load(_, _) => vec![],
            Op::Store(_, src) => vec![*src],
            Op::Put(src) => vec![*src],
            Op::Binary(_, _, lhs, rhs) => vec![*lhs, *rhs],
            Op::HardBinary(_, _, lhs, rhs) => vec![*lhs, *rhs],
            Op::Call(_, _, args) => args.clone(),
            Op::Cast(_, src, _) => vec![*src],
            Op::AllocDatum(_, _) => vec![],
            Op::DatumLoadVar(_, src, _) => vec![*src],
            Op::DatumStoreVar(dsrc, _, src) => vec![*dsrc, *src], // unsure about other local here
            Op::DatumCallProc(_, src, _, args) => {
                let mut v = args.clone();
                v.push(*src);
                v
            },
            Op::Throw(src) => vec![*src],
            Op::CatchException(_) => vec![],
            // hm, this should probably source all the captured vars..
            Op::Spawn(_, Some(delay)) => vec![*delay],
            Op::Spawn(_, None) => vec![],
            Op::Sleep(delay) => vec![*delay],
        }
    }

    // mightn't be the best place for these
    pub fn visit_dest(&mut self, mut f: impl FnMut(&mut LocalId)) {
        match self {
            Op::Literal(dst, _) => f(dst),
            Op::Load(dst, _) => f(dst),
            Op::Binary(dst, _, _, _) => f(dst),
            Op::HardBinary(dst, _, _, _) => f(dst),
            Op::Call(dst, _, _) => f(dst),
            Op::Cast(dst, _, _) => f(dst),
            Op::AllocDatum(dst, _) => f(dst),
            Op::DatumLoadVar(dst, _, _) => f(dst),
            Op::DatumCallProc(dst, _, _, _) => f(dst),
            Op::Noop => {},
            Op::MkVar(_) => {},
            Op::Store(_, _) => {},
            Op::Put(_) => {},
            Op::DatumStoreVar(_, _, _) => {},
            Op::Throw(_) => {},
            Op::CatchException(_) => {},
            Op::Spawn(_, _) => {},
            Op::Sleep(_) => {},
        }
    }

    pub fn visit_source(&mut self, mut f: impl FnMut(&mut LocalId)) {
        match self {
            Op::Store(_, src) => f(src),
            Op::Put(src) => f(src),
            Op::Binary(_, _, lhs, rhs) => {
                f(lhs);
                f(rhs)
            },
            Op::HardBinary(_, _, lhs, rhs) => {
                f(lhs);
                f(rhs)
            },
            Op::Call(_, _, args) => args.iter_mut().for_each(f),
            Op::Cast(_, src, _) => f(src),
            Op::DatumLoadVar(_, src, _) => f(src),
            Op::DatumStoreVar(dsrc, _, src) => {
                f(dsrc);
                f(src)
            },
            Op::DatumCallProc(_, src, _, args) => {
                f(src);
                args.iter_mut().for_each(f);
            },
            Op::Throw(src) => f(src),
            Op::Noop => {},
            Op::Literal(_, _) => {},
            Op::MkVar(_) => {},
            Op::Load(_, _) => {},
            Op::AllocDatum(_, _) => {},
            Op::CatchException(_) => {},
            // ditto
            Op::Spawn(_, Some(delay)) => {
                f(delay);
            },
            Op::Spawn(_, None) => {},
            Op::Sleep(delay) => {
                f(delay);
            },
        }
    }

    pub fn visit_var(&mut self, mut f: impl FnMut(&mut VarId)) {
        match self {
            Op::MkVar(var) => f(var),
            Op::Load(_, var) => f(var),
            Op::Store(var, _) => f(var),
            Op::CatchException(Some(var)) => f(var),
            _ => {},
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum Literal {
    Null,
    Num(f32),
    String(StringId),
    List,
}

impl Literal {
    pub fn get_ty(&self) -> Type {
        match self {
            Literal::Null => Type::Null,
            Literal::Num(_) => Type::Float,
            Literal::String(_) => Type::Ref(RefType::String),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum Terminator {
    Return,
    Jump(BlockId),
    Switch {
        discriminant: LocalId,
        branches: Vec<(u32, BlockId)>,
        default: BlockId,
    },
    TryCatch {
        try_block: BlockId,
        catch_block: BlockId,
    },
}

impl Terminator {
    pub fn visit_local(&mut self, mut f: impl FnMut(&mut LocalId)) {
        match self {
            Terminator::Switch {
                discriminant: disc,
                branches: _,
                default: _,
            } => {
                f(disc);
            },
            Terminator::Return | Terminator::Jump(_) | Terminator::TryCatch { .. } => {},
        }
    }
}
