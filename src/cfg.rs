use std::collections::HashMap;
use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};

newtype_index!(LocalId {pub idx});
newtype_index!(BlockId {pub idx});

#[derive(Debug)]
pub struct Local {
    pub id: LocalId,
    pub name: Option<String>,
}

#[derive(Debug)]
pub struct Proc {
    pub locals: IndexVec<LocalId, Local>,
    pub vars: HashMap<String, LocalId>,
    pub blocks: IndexVec<BlockId, Block>,

    pub next_block_id: usize,
}

impl<'a> Proc {
    pub fn new() -> Self {
        let mut new = Self {
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
            name: name.map(|s| s.to_string()),
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
