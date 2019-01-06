use std::collections::HashMap;
use crate::cfg;
use dreammaker::{ast, objtree};
use indexed_vec::Idx;

pub struct Builder<'a> {
    tree: &'a objtree::ObjectTree,
}

impl<'a> Builder<'a> {
    pub fn new(tree: &'a objtree::ObjectTree) -> Self {
        Self {
            tree: tree,
        }
    }

    pub fn build_procs(&self) -> HashMap<String, cfg::Proc> {
        self.tree.root().get().procs.iter().filter(|(name, procs)| {
            procs.value[0].body.len() != 0 // TODO REMOVE THIS
        }).map(|(name, procs)| {
            (name.clone(), ProcBuilder::build(&name, &procs.value[0]))
        }).collect()
    }
}

struct ProcBuilder<'a> {
    ast_proc: &'a objtree::ProcValue,
    vars: HashMap<String, cfg::LocalId>,
    proc: cfg::Proc,

    finished_blocks: Vec<cfg::Block>,
}

impl<'a> ProcBuilder<'a> {
    fn build(name: &str, ast_proc: &'a objtree::ProcValue) -> cfg::Proc {
        let mut builder = Self {
            ast_proc: ast_proc,
            vars: HashMap::new(),
            proc: cfg::Proc::new(name.to_string()),

            finished_blocks: Vec::new(),
        };

        builder.build_proc();

        builder.proc
    }

    fn build_proc(&mut self) {
        // finddecls
        for stmt in self.ast_proc.body.iter() {
            if let ast::Statement::Var(v) = stmt {
                self.proc.add_local(Some(&v.name));
            }
        }

        self.build_block(&self.ast_proc.body, None);

        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.proc.add_block(block);
        }

        self.proc.dot()
    }

    fn build_block(&mut self, stmts: &[ast::Statement], next_block: Option<cfg::BlockId>) -> cfg::BlockId {
        let mut block = self.proc.new_block();

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
                                    assert!(unary.len() == 0);
                                    assert!(follow.len() == 0);
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
                            let local = self.proc.add_local(None);
                            let res = self.build_expr(rhs, &mut block);
                            block.ops.push(cfg::Op::Mov(local, res));
                            match op {
                                ast::BinaryOp::LShift => {
                                    block.ops.push(cfg::Op::Put(local));
                                },
                                _ => unimplemented!(),
                            };
                        },

                        expr => {
                            self.build_expr(expr, &mut block);
                        }
                    }
                },

                ast::Statement::Return(val) => {
                    if let Some(val) = val {
                        let val_expr = self.build_expr(val, &mut block);
                        block.ops.push(cfg::Op::Mov(cfg::LocalId::new(0), val_expr));
                    }
                },

                ast::Statement::If(branches, else_branch) => {
                    let if_end = self.proc.new_block();

                    for (condition, body) in branches.iter() {
                        let cond_expr = self.build_expr(condition, &mut block);
                        let cond_local = self.proc.add_local(None);
                        block.ops.push(cfg::Op::Mov(cond_local, cond_expr));

                        let body_block_id = self.build_block(&body[..], Some(if_end.id));

                        let continuation = self.proc.new_block();

                        block.terminator = cfg::Terminator::Switch {
                            discriminant: cfg::Place::Local(cond_local),
                            branches: vec![(0, continuation.id)],
                            default: body_block_id,
                        };

                        self.finished_blocks.push(block);
                        block = continuation;
                    }

                    let mut next = if_end.id;
                    if let Some(body) = else_branch {
                        next = self.build_block(&body[..], Some(if_end.id));
                    }

                    // adds a redundant jump, but simplifies code.
                    // will be optimized out anyway
                    block.terminator = cfg::Terminator::Jump(next);

                    self.finished_blocks.push(block);
                    block = if_end;
                },

                ast::Statement::While(cond, body) => {
                    let mut cond_block = self.proc.new_block();
                    let while_end = self.proc.new_block();

                    let body_block_id = self.build_block(&body[..], Some(cond_block.id));

                    let cond_expr = self.build_expr(cond, &mut cond_block);
                    let cond_local = self.proc.add_local(None);
                    cond_block.ops.push(cfg::Op::Mov(cond_local, cond_expr));

                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cfg::Place::Local(cond_local),
                        branches: vec![(0, while_end.id)],
                        default: body_block_id,
                    };

                    block.terminator = cfg::Terminator::Jump(cond_block.id);

                    self.finished_blocks.push(block);
                    self.finished_blocks.push(cond_block);

                    block = while_end;
                },

                _ => unimplemented!(),
            }
        }

        if let Some(next) = next_block {
            block.terminator = cfg::Terminator::Jump(next);
        }

        let id = block.id;
        self.finished_blocks.push(block);
        id
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression, block: &mut cfg::Block) {
        let var_id = self.proc.lookup_var(var).unwrap();
        let asg_expr = self.build_expr(expr, block);
        block.ops.push(cfg::Op::Mov(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression, block: &mut cfg::Block) -> cfg::Expr {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let expr = self.build_term(term, block);
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);

                expr
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs, block);
                let rhs_expr = self.build_expr(rhs, block);
                let local = self.proc.add_local(None);

                match op {
                    ast::BinaryOp::Add => {
                        block.ops.push(cfg::Op::Add(local, lhs_expr, rhs_expr));
                    },

                    _ => unimplemented!("{:?}", op),
                }

                cfg::Expr::Place(cfg::Place::Local(local))
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_term(&mut self, term: &ast::Term, block: &mut cfg::Block) -> cfg::Expr {
        match term {
            ast::Term::Int(x) => cfg::Expr::Literal(cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => cfg::Expr::Literal(cfg::Literal::Num(*x)),
            ast::Term::Ident(var_name) => {
                let var_id = self.proc.lookup_var(var_name).unwrap();
                cfg::Expr::Place(cfg::Place::Local(var_id))
            },
            ast::Term::Call(name, args) => {
                let res = self.proc.add_local(None);

                let arg_exprs: Vec<cfg::Expr> = args.iter().map(|expr| self.build_expr(expr, block)).collect();

                block.ops.push(cfg::Op::Call(res, name.clone(), arg_exprs));

                cfg::Expr::Place(cfg::Place::Local(res))
            },
            _ => unimplemented!("{:?}", term),
        }
    }
}
