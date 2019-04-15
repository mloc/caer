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

    cur_block: cfg::Block,
    finished_blocks: Vec<cfg::Block>,
}

impl<'a> ProcBuilder<'a> {
    fn build(name: &str, ast_proc: &'a objtree::ProcValue) -> cfg::Proc {
        let mut proc = cfg::Proc::new(name.to_string());
        let scope = proc.new_scope(proc.global_scope);
        let start_block = proc.new_block(scope);

        let mut builder = Self {
            ast_proc: ast_proc,
            vars: HashMap::new(),
            proc: proc,

            cur_block: start_block,
            finished_blocks: Vec::new(),
        };

        builder.build_proc()
    }

    fn cur_block_done(&mut self, replacement: cfg::Block) {
        std::mem::replace(&mut self.cur_block, replacement);
    }

    fn build_proc(mut self) -> cfg::Proc {
        self.build_block(&self.ast_proc.body, self.proc.global_scope, None);

        self.finished_blocks.push(self.cur_block);
        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.proc.add_block(block);
        }

        self.proc.analyze();
        self.proc.dot();

        self.proc
    }

    fn build_block(&mut self, stmts: &[ast::Statement], parent_scope: cfg::ScopeId, next_block: Option<cfg::BlockId>) -> cfg::BlockId {
        let scope = self.proc.new_scope(parent_scope);
        let mut block = self.proc.new_block(scope);
        let entry_id = block.id;

        for stmt in stmts.iter() {
            println!("{:?}", stmt);
            self.build_stmt(stmt);
        }

        self.cur_block.scope_end = true;

        if let Some(next) = next_block {
            self.cur_block.terminator = cfg::Terminator::Jump(next);
        }

        entry_id
    }

    fn build_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Var(v) => {
                let local = self.proc.add_local(self.cur_block.scope, Some(&v.name), true);
                self.cur_block.ops.push(cfg::Op::MkVar(local));

                if let Some(expr) = &v.value {
                    self.build_assign(&v.name, expr);
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

                        self.build_assign(var, &*rhs);
                    },

                    ast::Expression::BinaryOp { op, lhs, rhs } => {
                        let res = self.build_expr(rhs);
                        match op {
                            ast::BinaryOp::LShift => {
                                self.cur_block.ops.push(cfg::Op::Put(res));
                            },
                            _ => unimplemented!(),
                        };
                    },

                    expr => {
                        self.build_expr(expr);
                    }
                }
            },

            ast::Statement::Return(val) => {
                if let Some(val) = val {
                    let val_expr = self.build_expr(val);
                    self.cur_block.ops.push(cfg::Op::Store(cfg::LocalId::new(0), val_expr));
                    // TODO make sure we check the rest of the ops somehow?
                    // create an unreachable block and continue? need a more robust builder
                    return;
                }
            },

            ast::Statement::If(branches, else_branch) => {
                let if_end = self.proc.new_block(self.cur_block.scope);

                // TODO clean up this scope creation with better builder
                for (condition, body) in branches.iter() {
                    let cond_scope = self.proc.new_scope(self.cur_block.scope);
                    let mut cond_block = self.proc.new_block(cond_scope);
                    cond_block.scope_end = true;

                    let cond_expr = self.build_expr(condition);

                    let body_block_id = self.build_block(&body[..], if_end.scope, Some(if_end.id));

                    let continuation = self.proc.new_block(cond_scope);

                    self.cur_block.terminator = cfg::Terminator::Jump(cond_block.id);
                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, continuation.id)],
                        default: body_block_id,
                    };

                    self.finished_blocks.push(cond_block);
                    self.cur_block_done(continuation);
                }

                let mut next = if_end.id;
                if let Some(body) = else_branch {
                    next = self.build_block(&body[..], if_end.scope, Some(if_end.id));
                }

                // adds a redundant jump, but simplifies code.
                // will be optimized out anyway
                self.cur_block.terminator = cfg::Terminator::Jump(next);

                self.cur_block_done(if_end);
            },

            ast::Statement::While(cond, body) => {
                let cond_scope = self.proc.new_scope(self.cur_block.scope);
                let mut cond_block = self.proc.new_block(cond_scope);
                cond_block.scope_end = true;
                let while_end = self.proc.new_block(self.cur_block.scope);

                let body_block_id = self.build_block(&body[..], self.cur_block.scope, Some(cond_block.id));

                let cond_expr = self.build_expr(cond);

                cond_block.terminator = cfg::Terminator::Switch {
                    discriminant: cond_expr,
                    branches: vec![(0, while_end.id)],
                    default: body_block_id,
                };

                self.cur_block.terminator = cfg::Terminator::Jump(cond_block.id);

                self.finished_blocks.push(cond_block);
                self.cur_block_done(while_end);
            },

            ast::Statement::ForLoop { init, test, inc, block } => {
                let loop_scope = self.proc.new_scope(self.cur_block.scope);
                let mut init_block = self.proc.new_block(loop_scope);

                if let Some(init_stmt) = init {

                }
            },

            _ => unimplemented!(),
        }
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression) {
        let var_id = self.proc.lookup_var(self.cur_block.scope, var).unwrap();
        let asg_expr = self.build_expr(expr);
        self.cur_block.ops.push(cfg::Op::Store(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression) -> cfg::LocalId {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let expr = self.build_term(term);
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);

                expr
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs);
                let rhs_expr = self.build_expr(rhs);
                let local = self.proc.add_local(self.cur_block.scope, None, false);

                let l_op = match op {
                    ast::BinaryOp::Add => BinaryOp::Add,
                    ast::BinaryOp::Sub => BinaryOp::Sub,
                    ast::BinaryOp::Mul => BinaryOp::Mul,
                    ast::BinaryOp::Div => BinaryOp::Div,
                    _ => unimplemented!("binary op {:?}", op),
                };

                self.cur_block.ops.push(cfg::Op::Binary(local, l_op, lhs_expr, rhs_expr));

                local
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_literal(&mut self, lit: cfg::Literal) -> cfg::LocalId {
        let local = self.proc.add_local(self.cur_block.scope, None, false);
        self.cur_block.ops.push(cfg::Op::Literal(local, lit));
        local
    }

    fn build_term(&mut self, term: &ast::Term) -> cfg::LocalId {
        match term {
            ast::Term::Int(x) => self.build_literal(cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => self.build_literal(cfg::Literal::Num(*x)),
            ast::Term::Ident(var_name) => {
                let var_id = self.proc.lookup_var(self.cur_block.scope, var_name).unwrap();
                let loaded = self.proc.add_local(self.cur_block.scope, None, false);
                self.cur_block.ops.push(cfg::Op::Load(loaded, var_id));
                loaded
            },
            ast::Term::Call(name, args) => {
                let res = self.proc.add_local(self.cur_block.scope, None, false);

                let arg_exprs: Vec<_> = args.iter().map(|expr| self.build_expr(expr)).collect();

                self.cur_block.ops.push(cfg::Op::Call(res, name.clone(), arg_exprs));

                res
            },
            _ => unimplemented!("{:?}", term),
        }
    }
}

/*struct BlockBuilder<'a> {
    pb: &'a mut ProcBuilder<'a>,
    block: cfg::Block,
}

impl<'a> BlockBuilder<'a> {
    fn new(pb: &'a mut ProcBuilder<'a>) -> Self {
        let scope = pb.proc.new_scope(pb.proc.global_scope);
        let mut block = pb.proc.new_block(scope);

        Self {
            pb: pb,
            block: block,
        }
    }

    fn push_op(&mut self, op: cfg::Op) {
        self.block.ops.push(op)
    }

    // TODO rename, bad bus name
    // TODO redo, probably inefficient as heck
    fn on_block<R>(&mut self, block: &'a mut cfg::Block, action: impl FnOnce(&mut BlockBuilder) -> R) {
        std::mem::swap(&mut self.block, block);
        let ret = action(self);
        std::mem::swap(&mut self.block, block);

        //ret
    }

    fn build_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Var(v) => {
                let local = self.pb.proc.add_local(self.scope, Some(&v.name), true);
                self.push_op(cfg::Op::MkVar(local));

                if let Some(expr) = &v.value {
                    self.build_assign(&v.name, expr);
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

                        self.build_assign(var, &*rhs);
                    },

                    ast::Expression::BinaryOp { op, lhs, rhs } => {
                        let res = self.build_expr(rhs);
                        match op {
                            ast::BinaryOp::LShift => {
                                self.push_op(cfg::Op::Put(res));
                            },
                            _ => unimplemented!(),
                        };
                    },

                    expr => {
                        self.build_expr(expr);
                    }
                }
            },

            ast::Statement::Return(val) => {
                if let Some(val) = val {
                    let val_expr = self.build_expr(val);
                    self.push_op(cfg::Op::Store(cfg::LocalId::new(0), val_expr));
                    // TODO make sure we check the rest of the ops somehow?
                    // create an unreachable block and continue? need a more robust builder
                    return;
                }
            },

            ast::Statement::If(branches, else_branch) => {
                let if_end = self.pb.proc.new_block(self.block.scope);

                // TODO clean up this scope creation with better builder
                for (condition, body) in branches.iter() {
                    let cond_scope = self.pb.proc.new_scope(self.block.scope);
                    let mut cond_block = self.pb.proc.new_block(cond_scope);
                    cond_block.scope_end = true;

                    let cond_expr = self.build_expr(condition, &mut cond_block);

                    let body_block_id = self.build_block(&body[..], scope, Some(if_end.id));

                    let continuation = self.proc.new_block(cond_scope);

                    block.terminator = cfg::Terminator::Jump(cond_block.id);
                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, continuation.id)],
                        default: body_block_id,
                    };

                    self.finished_blocks.push(cond_block);
                    self.finished_blocks.push(block);
                    block = continuation;
                }

                let mut next = if_end.id;
                if let Some(body) = else_branch {
                    next = self.build_block(&body[..], scope, Some(if_end.id));
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

                cond_block.terminator = cfg::Terminator::Switch {
                    discriminant: cond_expr,
                    branches: vec![(0, while_end.id)],
                    default: body_block_id,
                };

                block.terminator = cfg::Terminator::Jump(cond_block.id);

                self.finished_blocks.push(block);
                self.finished_blocks.push(cond_block);

                block = while_end;
            },

            ast::Statement::ForLoop { init, test, inc, block } => {
                let loop_scope = self.proc.new_scope(scope);
                let mut init_block = self.proc.new_block(loop_scope);

                if let Some(init_stmt) = init {

                }
            },

            _ => unimplemented!(),
        }
    }
}*/
