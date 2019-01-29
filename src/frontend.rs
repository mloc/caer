use std::collections::HashMap;
use crate::cfg;
use dreammaker::{ast, objtree};
use indexed_vec::Idx;
use ludo::op::BinaryOp;

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
        self.build_block(&self.ast_proc.body, self.proc.global_scope, None);

        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.proc.add_block(block);
        }

        self.proc.analyze();
        self.proc.dot();
    }

    fn build_block(&mut self, stmts: &[ast::Statement], parent_scope: cfg::ScopeId, next_block: Option<cfg::BlockId>) -> cfg::BlockId {
        let scope = self.proc.new_scope(parent_scope);
        let mut block = self.proc.new_block(scope);

        for stmt in stmts.iter() {
            println!("{:?}", stmt);
            match stmt {
                ast::Statement::Var(v) => {
                    let local = self.proc.add_local(scope, Some(&v.name), true);
                    block.ops.push(cfg::Op::MkVar(local));

                    if let Some(expr) = &v.value {
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
                            let local = self.proc.add_local(scope, None, false);
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
                        block.ops.push(cfg::Op::Store(cfg::LocalId::new(0), val_expr));
                        // TODO make sure we check the rest of the ops somehow?
                        // create an unreachable block and continue? need a more robust builder
                        break;
                    }
                },

                ast::Statement::If(branches, else_branch) => {
                    let mut if_end = self.proc.new_block(scope);
                    let mut if_cleanup = if_end.id;

                    // TODO clean up this scope creation with better builder
                    for (condition, body) in branches.iter() {
                        // scope to contain (effective) if/else pair
                        let cond_scope = self.proc.new_scope(block.scope);
                        let mut cond_block = self.proc.new_block(cond_scope);

                        // end block
                        let mut cond_cleanup = self.proc.new_block(cond_scope);
                        cond_cleanup.scope_end = true;
                        cond_cleanup.terminator = cfg::Terminator::Jump(if_cleanup);
                        if_cleanup = cond_cleanup.id;

                        let cond_expr = self.build_expr(condition, &mut cond_block);

                        let body_block_id = self.build_block(&body[..], scope, Some(if_cleanup));

                        let continuation = self.proc.new_block(cond_scope);

                        block.terminator = cfg::Terminator::Jump(cond_block.id);
                        cond_block.terminator = cfg::Terminator::Switch {
                            discriminant: cond_expr,
                            branches: vec![(0, continuation.id)],
                            default: body_block_id,
                        };

                        self.finished_blocks.push(cond_block);
                        self.finished_blocks.push(cond_cleanup);
                        self.finished_blocks.push(block);
                        block = continuation;
                    }

                    let mut next = if_end.id;
                    if let Some(body) = else_branch {
                        next = self.build_block(&body[..], scope, Some(if_cleanup));
                    }

                    // adds a redundant jump, but simplifies code.
                    // will be optimized out anyway
                    block.terminator = cfg::Terminator::Jump(next);

                    self.finished_blocks.push(block);
                    block = if_end;
                },

                ast::Statement::While(cond, body) => {
                    let cond_scope = self.proc.new_scope(scope);
                    let mut cond_block = self.proc.new_block(cond_scope);
                    cond_block.scope_end = true;
                    let while_end = self.proc.new_block(scope);

                    let body_block_id = self.build_block(&body[..], scope, Some(cond_block.id));

                    let cond_expr = self.build_expr(cond, &mut cond_block);
                    let cond_local = self.proc.add_local(cond_scope, None, false);
                    cond_block.ops.push(cfg::Op::Mov(cond_local, cond_expr));

                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_local,
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

        block.scope_end = true;

        if let Some(next) = next_block {
            block.terminator = cfg::Terminator::Jump(next);
        }

        let id = block.id;
        self.finished_blocks.push(block);
        id
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression, block: &mut cfg::Block) {
        let var_id = self.proc.lookup_var(block.scope, var).unwrap();
        let asg_expr = self.build_expr(expr, block);
        block.ops.push(cfg::Op::Store(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression, block: &mut cfg::Block) -> cfg::LocalId {
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
                let local = self.proc.add_local(block.scope, None, false);

                let l_op = match op {
                    ast::BinaryOp::Add => BinaryOp::Add,
                    ast::BinaryOp::Sub => BinaryOp::Sub,
                    ast::BinaryOp::Mul => BinaryOp::Mul,
                    ast::BinaryOp::Div => BinaryOp::Div,
                    _ => unimplemented!("binary op {:?}", op),
                };

                block.ops.push(cfg::Op::Binary(local, l_op, lhs_expr, rhs_expr));

                local
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_literal(&mut self, lit: cfg::Literal, block: &mut cfg::Block) -> cfg::LocalId {
        let local = self.proc.add_local(block.scope, None, false);
        block.ops.push(cfg::Op::Literal(local, lit));
        local
    }

    fn build_term(&mut self, term: &ast::Term, block: &mut cfg::Block) -> cfg::LocalId {
        match term {
            ast::Term::Int(x) => self.build_literal(cfg::Literal::Num(*x as f32), block),
            ast::Term::Float(x) => self.build_literal(cfg::Literal::Num(*x), block),
            ast::Term::Ident(var_name) => {
                let var_id = self.proc.lookup_var(block.scope, var_name).unwrap();
                let loaded = self.proc.add_local(block.scope, None, false);
                block.ops.push(cfg::Op::Load(loaded, var_id));
                loaded
            },
            ast::Term::Call(name, args) => {
                let res = self.proc.add_local(block.scope, None, false);

                let arg_exprs: Vec<_> = args.iter().map(|expr| self.build_expr(expr, block)).collect();

                block.ops.push(cfg::Op::Call(res, name.clone(), arg_exprs));

                res
            },
            _ => unimplemented!("{:?}", term),
        }
    }
}
