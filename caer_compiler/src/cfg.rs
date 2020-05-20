use std::collections::{HashMap, HashSet};
use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};
use std::fs::{self, File};
use std::io::{Seek, SeekFrom, Write};
use std::cmp;
use caer_runtime::string_table::{StringTable, StringId};
use caer_runtime;
use crate::ty;
use dot;

newtype_index!(LocalId {pub idx});
newtype_index!(BlockId {pub idx});
newtype_index!(ScopeId {pub idx});

// this should be renamed and moved out of env.
#[derive(Debug)]
pub struct Environment {
    // is this bad? yes
    // but it's easy
    // TODO: do something less tightly coupled
    pub string_table: StringTable,
    pub rt_env: caer_runtime::environment::Environment,
    pub procs: HashMap<String, Proc>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            string_table: StringTable::new(),
            rt_env: caer_runtime::environment::Environment::new(),
            procs: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Local {
    pub id: LocalId,
    pub ty: ty::Complex,
    pub movable: bool,
    pub var:  bool,
    pub param: bool,
    pub name: Option<String>,
    pub construct_scope: ScopeId,
    // if a value is moved, it won't be destructed with this local
    pub destruct_scope: Option<ScopeId>,
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

#[derive(Debug)]
pub struct Proc {
    pub name: String,
    pub env_id: caer_runtime::environment::ProcId,

    pub locals: IndexVec<LocalId, Local>,
    pub vars: HashMap<String, LocalId>,
    pub blocks: IndexVec<BlockId, Block>,
    pub scopes: IndexVec<ScopeId, Scope>,

    pub global_scope: ScopeId,

    pub next_block_id: usize,
}

impl<'a> Proc {
    pub fn new(name: String) -> Self {
        // ids map to indices in the scopes list; we can assume this scope will have id 0
        // TODO sounder way to handle this + locals?
        let global_scope = Scope::new(ScopeId::new(0), None);
        let mut scopes = IndexVec::new();
        scopes.push(global_scope);

        let mut new = Self {
            name: name,
            // TODO: change this, very bad
            env_id: caer_runtime::environment::ProcId::new(0),

            locals: IndexVec::new(),
            vars: HashMap::new(),
            blocks: IndexVec::new(),
            scopes: scopes,

            global_scope: ScopeId::new(0),

            next_block_id: 0,
        };

        new.add_local(new.global_scope, ty::Complex::Any, None, true, false); // return
        new
    }

    pub fn add_local(&mut self, scope: ScopeId, ty: ty::Complex, name: Option<&str>, var: bool, param: bool) -> LocalId {
        let id = LocalId::new(self.locals.len());

        let local = Local {
            id: id,
            ty: ty,
            movable: false,
            var: var,
            param: param,
            name: Some(name.map_or_else(|| {format!("local_{}", id.index())}, |s| {format!("var_{}", s.to_string())})),
            construct_scope: scope,
            destruct_scope: Some(scope),
        };

        self.locals.push(local);
        self.scopes[scope].locals.push(id);
        self.scopes[scope].destruct_locals.insert(id);

        if var {
            if let Some(var) = name {
                self.vars.insert(var.into(), id);
                self.scopes[scope].vars.insert(var.into(), id);
            }
        }

        id
    }

    pub fn new_block(&mut self, scope: ScopeId) -> Block {
        let id = BlockId::new(self.next_block_id);
        self.next_block_id += 1;
        self.scopes[scope].blocks.push(id);
        Block::new(id, scope)
    }

    pub fn add_block(&mut self, block: Block) {
        assert!(block.id == BlockId::new(self.blocks.len()));
        self.blocks.push(block);
    }

    // scope tree search to find matching var
    pub fn lookup_var(&self, root_scope: ScopeId, var: &str) -> Option<LocalId> {
        let var = var.to_string();
        let mut cur_scope = root_scope;
        loop {
            let scope = &self.scopes[cur_scope];
            match scope.vars.get(&var) {
                Some(local) => return Some(*local),
                None => match scope.parent {
                    Some(parent) => cur_scope = parent,
                    None => break,
                }
            }
        }

        None
    }

    pub fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        let id = ScopeId::new(self.scopes.len());
        self.scopes[parent].children.push(id);
        let scope = Scope::new(id, Some((parent, self.scopes[parent].depth)));
        self.scopes.push(scope);
        id
    }

    // analysis procs
    // TODO split up, move out of Proc maybe, and track as we build
    // TODO a smarter analysis method; this works but misses a lot
    pub fn analyze(&mut self) {
        let mut flow: IndexVec<_, _> = self.locals.iter().map(|l| {
            LocalFlow::new(l)
        }).collect();

        for block in self.blocks.iter() {
            for op in block.ops.iter() {
                match op {
                    Op::Literal(_, _) => {
                        // hmmm
                    },

                    Op::MkVar(_) => {},

                    Op::Load(_, src) => {
                        flow[*src].takes += 1;
                    },

                    Op::Store(_, _) => {},

                    Op::Put(src) => {
                        flow[*src].reads += 1;
                    },

                    Op::Binary(_, _, lhs, rhs) => {
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
                    }
                }
            }

            match block.terminator {
                Terminator::Switch { discriminant, branches: _, default: _ } => {
                    flow[discriminant].reads += 1;
                },
                _ => {},
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

    pub fn dot(&self) {
        // TODO bad, for now we assume procs have unique names
        fs::create_dir_all("dbgout/dot/tino_cfg/").unwrap();
        let mut f = File::create(format!("dbgout/dot/tino_cfg/cfg_{}.dot", self.name)).unwrap();
        dot::render(self, &mut f).unwrap();

        f.seek(SeekFrom::End(-2)).unwrap();
        //write!(f, "newrank=true;\n").unwrap();
        self.dot_scope_cluster(&mut f, self.global_scope);
        write!(&mut f, "}}\n").unwrap();
    }

    fn dot_scope_cluster(&self, f: &mut File, scope_id: ScopeId) {
        let scope = &self.scopes[scope_id];
        let locals = scope.locals.iter().map(|l| l.index().to_string()).collect::<Vec<_>>().join(", ");
        write!(f, "subgraph cluster_scope{} {{\n", scope_id.index()).unwrap();
        write!(f, "label=\"scope {} [{}]\";\n", scope.id.index(), locals).unwrap();
        for block_id in scope.blocks.iter() {
            write!(f, "block{};\n", block_id.index()).unwrap();
        }
        for child in scope.children.iter() {
            self.dot_scope_cluster(f, *child);
        }
        write!(f, "graph[style=dotted];}}\n").unwrap();
    }
}

impl<'a> dot::Labeller<'a, BlockId, (BlockId, BlockId, String)> for Proc {
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
            Terminator::Switch { discriminant, branches: _, default: _ } => format!("switch {:?}", discriminant),
        };

        let end_str = if block.scope_end {
            "END SCOPE\\l|"
        } else {
            ""
        };

        dot::LabelText::escaped(
            format!("{{{}\\l|{}{{{}}}}}", block.ops.iter()
                .map(|op| format!("{:?}", op).replace("\\", "\\\\"))
                .collect::<Vec<_>>()
                .join("\\l"),
                end_str,
                term_str))
    }

    fn edge_label(&'a self, e: &(BlockId, BlockId, String)) -> dot::LabelText<'a> {
        dot::LabelText::label(e.2.clone())
    }

    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new(&self.name).unwrap()
    }
}

impl<'a> dot::GraphWalk<'a, BlockId, (BlockId, BlockId, String)> for Proc {
    fn nodes(&self) -> dot::Nodes<'a, BlockId> {
        self.blocks.iter().map(|b| b.id).collect()
    }

    fn edges(&self) -> dot::Edges<'a, (BlockId, BlockId, String)> {
        self.blocks.iter().flat_map(|block| {
            match &block.terminator {
                Terminator::Return => {vec![]},
                Terminator::Jump(target) => vec![(block.id, *target, "".into())],
                Terminator::Switch { discriminant: _, branches, default } => {
                    let mut out = vec![(block.id, *default, "default".into())];

                    for (val, target) in branches.iter() {
                        out.push((block.id, *target, val.to_string()));
                    }

                    out
                },
            }
        }).collect::<Vec<_>>().into()
    }

    fn source(&self, e: &(BlockId, BlockId, String)) -> BlockId {
        e.0
    }

    fn target(&self, e: &(BlockId, BlockId, String)) -> BlockId {
        e.1
    }
}

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    pub ops: Vec<Op>,
    pub terminator: Terminator,
    pub scope: ScopeId,
    pub scope_end: bool,
}

impl Block {
    pub fn new(id: BlockId, scope: ScopeId) -> Self {
        Self {
            id: id,
            ops: Vec::new(),
            terminator: Terminator::Return,
            scope: scope,
            scope_end: false,
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub depth: i32,
    pub locals: Vec<LocalId>,
    pub destruct_locals: HashSet<LocalId>,
    pub vars: HashMap<String, LocalId>,
    pub blocks: Vec<BlockId>,
}

impl Scope {
    fn new(id: ScopeId, parent: Option<(ScopeId, i32)>) -> Self {
        Self {
            id: id,
            parent: parent.map(|(id, _)| id),
            children: Vec::new(),
            depth: parent.map_or(0, |(_, d)| d + 1),
            locals: Vec::new(),
            destruct_locals: HashSet::new(),
            vars: HashMap::new(),
            blocks: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum Op {
    Literal(LocalId, Literal),

    MkVar(LocalId),
    Load(LocalId, LocalId),
    Store(LocalId, LocalId),

    Put(LocalId),
    Binary(LocalId, caer_runtime::op::BinaryOp, LocalId, LocalId),
    // TODO: STRINGID
    // TODO: use + ref a procid
    //Call(LocalId, StringId, Vec<LocalId>, Vec<(StringId, LocalId)>),
    Call(LocalId, String, Vec<LocalId>),

    Cast(LocalId, LocalId, ty::Primitive),
}

#[derive(Debug)]
pub enum Literal {
    Null,
    Num(f32),
    String(StringId),
    List,
}

impl Literal {
    pub fn get_ty(&self) -> ty::Complex {
        match self {
            Literal::Null => ty::Primitive::Null.into(),
            Literal::Num(_) => ty::Primitive::Float.into(),
            Literal::String(_) => ty::Primitive::String.into(),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum Terminator {
    Return,
    Jump(BlockId),
    Switch {
        discriminant: LocalId,
        branches: Vec<(u32, BlockId)>,
        default: BlockId,
    },
}
