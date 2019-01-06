use std::collections::HashMap;
use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};
use std::fs::{self, File};
use dot;

newtype_index!(LocalId {pub idx});
newtype_index!(BlockId {pub idx});

#[derive(Debug)]
pub struct Local {
    pub id: LocalId,
    pub name: Option<String>,
}

#[derive(Debug)]
pub struct Proc {
    pub name: String,

    pub locals: IndexVec<LocalId, Local>,
    pub vars: HashMap<String, LocalId>,
    pub blocks: IndexVec<BlockId, Block>,

    pub next_block_id: usize,
}

impl<'a> Proc {
    pub fn new(name: String) -> Self {
        let mut new = Self {
            name: name,

            locals: IndexVec::new(),
            vars: HashMap::new(),
            blocks: IndexVec::new(),

            next_block_id: 0,
        };
        new.add_local(None); // return
        new
    }

    pub fn add_local(&mut self, name: Option<&str>) -> LocalId {
        let id = LocalId::new(self.locals.len());

        let local = Local {
            id: id,
            name: Some(name.map_or_else(|| {format!("local_{}", id.index())}, |s| {format!("var_{}", s.to_string())})),
        };

        self.locals.push(local);

        if let Some(var) = name {
            self.vars.insert(var.into(), id);
        }

        id
    }

    pub fn new_block(&mut self) -> Block {
        let id = self.next_block_id;
        self.next_block_id += 1;
        Block::new(BlockId::new(id))
    }

    pub fn add_block(&mut self, block: Block) {
        assert!(block.id == BlockId::new(self.blocks.len()));
        self.blocks.push(block);
    }

    pub fn lookup_var(&self, var: &str) -> Option<LocalId> {
        self.vars.get(var).map(|id| *id)
    }

    pub fn dot(&self) {
        // TODO bad, for now we assume procs have unique names
        fs::create_dir_all("dotout/").unwrap();
        let mut f = File::create(format!("dotout/cfg_{}.dot", self.name)).unwrap();
        dot::render(self, &mut f).unwrap();
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

        dot::LabelText::escaped(
            format!("{{{}\\l|{{{}}}}}", block.ops.iter()
                .map(|op| format!("{:?}", op).replace("\\", "\\\\"))
                .collect::<Vec<_>>()
                .join("\\l"),
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
}

impl Block {
    pub fn new(id: BlockId) -> Self {
        Self {
            id: id,
            ops: Vec::new(),
            terminator: Terminator::Return,
        }
    }
}

#[derive(Debug)]
pub enum Op {
    Mov(LocalId, Expr),
    Put(LocalId),
    Add(LocalId, Expr, Expr),
    Call(LocalId, String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Literal {
    Null,
    Num(f32),
    String(String),
    List,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Place(Place),
}

#[derive(Debug)]
pub enum Place {
    Local(LocalId),
    Global(u32),
}

#[derive(Debug)]
pub enum Terminator {
    Return,
    Jump(BlockId),
    Switch {
        discriminant: Place,
        branches: Vec<(u32, BlockId)>,
        default: BlockId,
    },
}
