use std::collections::HashMap;
use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};

impl Proc {
    pub fn new() -> Self {
        let mut new = Self {
            locals: IndexVec::new(),
            vars: HashMap::new(),
            blocks: Vec::new(),
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

    pub fn build_block(&mut self, stmts: &[ast::Statement]) -> Block {
        let mut block = Block::new();

        for stmt in stmts.iter() {
            println!("{:?}", stmt);
            match stmt {
                ast::Statement::Var(v) => {
                    if let Some(ref expr) = v.value {
                        self.build_assign(&v.name, expr, &mut block);
                    }
                },

                ast::Statement::Expr(expr) => {
                    match expr {
                        ast::Expression::AssignOp { op, lhs, rhs } => {
                            let var = match lhs.as_ref() {
                                ast::Expression::Base { unary, term, follow } => {
                                    match term {
                                        ast::Term::Ident(ref s) => s,
                                        _ => unimplemented!(),
                                    }
                                },

                                _ => unimplemented!(),
                            };

                            self.build_assign(var, &*rhs, &mut block);
                        },

                        ast::Expression::BinaryOp { op, lhs, rhs } => {
                            let local = self.add_local(None);
                            let res = self.build_expr(rhs, &mut block);
                            block.ops.push(Op::Mov(local, res));
                            match op {
                                ast::BinaryOp::LShift => {
                                    block.ops.push(Op::Put(local));
                                },
                                _ => unimplemented!(),
                            };
                        },

                        _ => unimplemented!(),
                    }
                },

                _ => unimplemented!(),
            }
        }

        block
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression, block: &mut Block) {
        let var_id = self.lookup_var(var).unwrap();
        let asg_expr = self.build_expr(expr, block);
        block.ops.push(Op::Mov(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression, block: &mut Block) -> Expr {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let mut expr = self.build_term(term, block);
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);

                expr
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs, block);
                let rhs_expr = self.build_expr(rhs, block);
                let local = self.add_local(None);

                match op {
                    ast::BinaryOp::Add => {
                        block.ops.push(Op::Add(local, lhs_expr, rhs_expr));
                    },

                    _ => unimplemented!("{:?}", op),
                }

                Expr::Place(Place::Local(local))
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_term(&mut self, term: &ast::Term, block: &mut Block) -> Expr {
        match term {
            ast::Term::Int(x) => Expr::Literal(Literal::Num(*x as f32)),
            ast::Term::Float(x) => Expr::Literal(Literal::Num(*x)),
            ast::Term::Ident(var_name) => {
                let var_id = self.lookup_var(var_name).unwrap();
                Expr::Place(Place::Local(var_id))
            },
            ast::Term::Call(name, args) => {
                let res = self.add_local(None);

                let arg_exprs: Vec<Expr> = args.iter().map(|expr| self.build_expr(expr, block)).collect();

                block.ops.push(Op::Call(res, name.clone(), arg_exprs));

                Expr::Place(Place::Local(res))
            },
            _ => unimplemented!("{:?}", term),
        }
    }

    fn lookup_var(&self, var: &str) -> Option<LocalId> {
        self.vars.get(var).map(|id| *id)
    }
}

newtype_index!(LocalId {pub idx});

#[derive(Debug)]
pub struct Local {
    pub id: LocalId,
    pub name: Option<String>,
}

#[derive(Debug)]
pub struct Proc {
    pub locals: IndexVec<LocalId, Local>,
    pub vars: HashMap<String, LocalId>,

    pub blocks: Vec<Block>,
}

impl Proc {
}

#[derive(Debug)]
pub struct Block {
    pub ops: Vec<Op>,
    pub terminator: Terminator,
}

impl Block {
    fn new() -> Self {
        Self {
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
    Jump(u32),
    Switch {
        discriminant: Place,
        branches: Vec<(Literal, u32)>,
        default: Option<u32>,
    },
}
