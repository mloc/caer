use std::collections::HashMap;
use std::borrow::Cow;
use crate::cfg;
use dreammaker::{ast, objtree};
use indexed_vec::Idx;
use caer_runtime::op::BinaryOp;
use caer_runtime::string_table::StringId;
use crate::ty;

pub struct Builder<'a> {
    tree: &'a objtree::ObjectTree,
    env: cfg::Environment,
}
impl<'a> Builder<'a> {
    pub fn build(tree: &'a objtree::ObjectTree) -> cfg::Environment {
        let mut builder = Self {
            tree: tree,
            env: cfg::Environment::new(),
        };

        builder.build_procs();

        builder.env
    }

    fn build_procs(&mut self) {
        self.env.procs = self.tree.root().get().procs.iter().filter(|(name, procs)| {
            match &procs.main_value().code {
                objtree::Code::Present(_) => true,
                objtree::Code::Invalid(err) => panic!("oh no dm error {:?}", err),
                objtree::Code::Builtin => false,
                objtree::Code::Disabled => panic!("woop woop procs disabled"),
            }
            //procs.value[0].body.len() != 0 // TODO REMOVE THIS
        }).map(|(name, procs)| {
            (name.clone(), ProcBuilder::build(self, &name, &procs.value[0]))
        }).collect();
    }

    fn add_string(&mut self, s: impl Into<Cow<'a, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}

struct ProcBuilder<'a, 'b> {
    builder: &'a mut Builder<'b>,
    ast_proc: &'a objtree::ProcValue,
    vars: HashMap<String, cfg::LocalId>,
    proc: cfg::Proc,

    finished_blocks: Vec<cfg::Block>,
}

impl<'a, 'b> ProcBuilder<'a, 'b> {
    fn build(builder: &'a mut Builder<'b>, name: &str, ast_proc: &'a objtree::ProcValue) -> cfg::Proc {
        let mut proc = cfg::Proc::new(name.to_string());
        let scope = proc.new_scope(proc.global_scope);

        let mut pb = Self {
            builder: builder,
            ast_proc: ast_proc,
            vars: HashMap::new(),
            proc: proc,

            finished_blocks: Vec::new(),
        };

        pb.build_proc()
    }

    fn finalize_block(&mut self, block: cfg::Block) {
        self.finished_blocks.push(block);
    }

    fn build_proc(mut self) -> cfg::Proc {
        let body = if let objtree::Code::Present(ref b) = self.ast_proc.code {
            b
        } else {
            panic!("not present")
        };
        self.build_block(body.as_slice(), self.proc.global_scope, None);

        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.proc.add_block(block);
        }

        self.proc.analyze();
        self.proc.dot();

        self.proc
    }

    fn build_block(&mut self, stmts: &[ast::Spanned<ast::Statement>], parent_scope: cfg::ScopeId, next_block: Option<cfg::BlockId>) -> cfg::BlockId {
        let scope = self.proc.new_scope(parent_scope);

        let mut builder = BlockBuilder::new(self, scope);

        for stmt in stmts.iter() {
            println!("{:?}", stmt);
            builder.build_stmt(&stmt.elem);
        }

        let block_id = builder.root_block_id;
        let mut block = builder.done();

        block.scope_end = true;

        if let Some(next) = next_block {
            block.terminator = cfg::Terminator::Jump(next);
        }

        self.finalize_block(block);

        block_id
    }
}

struct BlockBuilder<'a, 'p, 'b> {
    pb: &'a mut ProcBuilder<'p, 'b>,
    block: cfg::Block,
    root_block_id: cfg::BlockId,
}

impl<'a, 'p, 'b> BlockBuilder<'a, 'p, 'b> {
    fn new(pb: &'a mut ProcBuilder<'p, 'b>, scope: cfg::ScopeId) -> Self {
        let mut block = pb.proc.new_block(scope);

        Self {
            pb: pb,
            root_block_id: block.id,
            block: block,
        }
    }

    fn done(self) -> cfg::Block {
        self.block
    }

    fn push_op(&mut self, op: cfg::Op) {
        self.block.ops.push(op)
    }

    // TODO rename, bad bus name
    // TODO redo, probably inefficient as heck, just figure out the lifetimes dude
    fn on_block<R>(&mut self, block: &mut cfg::Block, action: impl FnOnce(&mut BlockBuilder) -> R) -> R {
        std::mem::swap(&mut self.block, block);
        let ret = action(self);
        std::mem::swap(&mut self.block, block);

        ret
    }

    // TODO rename, more bad bus name
    fn cur_block_done(&mut self, replacement: cfg::Block) {
        let mut swp = replacement;
        std::mem::swap(&mut self.block, &mut swp);
        self.pb.finalize_block(swp);
    }


    fn build_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Var(v) => {
                let local = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, Some(&v.name), true);
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
                                match term.elem {
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

            ast::Statement::If { arms, else_arm } => {
                let if_end = self.pb.proc.new_block(self.block.scope);

                // TODO clean up this scope creation with better builder
                for (condition, body) in arms.iter() {
                    let cond_scope = self.pb.proc.new_scope(self.block.scope);
                    let mut cond_block = self.pb.proc.new_block(cond_scope);
                    cond_block.scope_end = true;

                    let cond_expr = self.on_block(&mut cond_block, |bb| {
                        bb.build_expr(&condition.elem)
                    });

                    let body_block_id = self.pb.build_block(&body[..], self.block.scope, Some(if_end.id));

                    let continuation = self.pb.proc.new_block(self.block.scope);

                    self.block.terminator = cfg::Terminator::Jump(cond_block.id);
                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, continuation.id)],
                        default: body_block_id,
                    };

                    self.pb.finalize_block(cond_block);
                    self.cur_block_done(continuation);
                }

                let mut next = if_end.id;
                if let Some(body) = else_arm {
                    next = self.pb.build_block(&body[..], self.block.scope, Some(if_end.id));
                }

                // adds a redundant jump, but simplifies code.
                // will be optimized out anyway
                self.block.terminator = cfg::Terminator::Jump(next);

                self.cur_block_done(if_end);
            },

            ast::Statement::While { condition, block } => {
                let cond_scope = self.pb.proc.new_scope(self.block.scope);
                let mut cond_block = self.pb.proc.new_block(cond_scope);
                cond_block.scope_end = true;
                let while_end = self.pb.proc.new_block(self.block.scope);

                let body_block_id = self.pb.build_block(&block[..], self.block.scope, Some(cond_block.id));

                let cond_expr = self.on_block(&mut cond_block, |bb| {
                    bb.build_expr(condition)
                });

                cond_block.terminator = cfg::Terminator::Switch {
                    discriminant: cond_expr,
                    branches: vec![(0, while_end.id)],
                    default: body_block_id,
                };

                self.block.terminator = cfg::Terminator::Jump(cond_block.id);

                self.pb.finalize_block(cond_block);
                self.cur_block_done(while_end);
            },

            ast::Statement::ForLoop { init, test, inc, block } => {
                let loop_scope = self.pb.proc.new_scope(self.block.scope);
                let mut init_block = self.pb.proc.new_block(loop_scope);
                let mut cond_block = self.pb.proc.new_block(loop_scope);
                let mut inc_block = self.pb.proc.new_block(loop_scope);
                let mut exit_block = self.pb.proc.new_block(loop_scope);
                exit_block.scope_end = true;

                if let Some(ref init_stmt) = init {
                    self.on_block(&mut init_block, |bb| {
                        bb.build_stmt(init_stmt);
                    });
                }

                let body_id = self.pb.build_block(block, loop_scope, Some(inc_block.id));

                if let Some(ref test_expr) = test {
                    let cond_expr = self.on_block(&mut cond_block, |bb| {
                        bb.build_expr(test_expr)
                    });
                    cond_block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, exit_block.id)],
                        default: body_id,
                    };
                } else {
                    cond_block.terminator = cfg::Terminator::Jump(body_id);
                }

                if let Some(ref inc_stmt) = inc {
                    self.on_block(&mut inc_block, |bb| {
                        bb.build_stmt(inc_stmt);
                    });
                }

                init_block.terminator = cfg::Terminator::Jump(cond_block.id);
                inc_block.terminator = cfg::Terminator::Jump(cond_block.id);

                let continuation = self.pb.proc.new_block(self.block.scope);
                self.block.terminator = cfg::Terminator::Jump(init_block.id);
                exit_block.terminator = cfg::Terminator::Jump(continuation.id);

                self.pb.finalize_block(init_block);
                self.pb.finalize_block(cond_block);
                self.pb.finalize_block(inc_block);
                self.pb.finalize_block(exit_block);
                self.cur_block_done(continuation);
            },

            _ => unimplemented!(),
        }
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression) {
        let var_id = self.pb.proc.lookup_var(self.block.scope, var).unwrap();
        let asg_expr = self.build_expr(expr);
        self.push_op(cfg::Op::Store(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression) -> cfg::LocalId {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let expr = self.build_term(&term.elem);
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);

                expr
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs);
                let rhs_expr = self.build_expr(rhs);
                let local = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);

                let l_op = match op {
                    ast::BinaryOp::Add => BinaryOp::Add,
                    ast::BinaryOp::Sub => BinaryOp::Sub,
                    ast::BinaryOp::Mul => BinaryOp::Mul,
                    ast::BinaryOp::Div => BinaryOp::Div,
                    _ => unimplemented!("binary op {:?}", op),
                };

                self.push_op(cfg::Op::Binary(local, l_op, lhs_expr, rhs_expr));

                local
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_literal(&mut self, lit: cfg::Literal) -> cfg::LocalId {
        let local = self.pb.proc.add_local(self.block.scope, lit.get_ty(), None, false);
        self.push_op(cfg::Op::Literal(local, lit));
        local
    }

    fn build_term(&mut self, term: &ast::Term) -> cfg::LocalId {
        match term {
            ast::Term::Int(x) => self.build_literal(cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => self.build_literal(cfg::Literal::Num(*x)),
            ast::Term::String(s) => {
                // TODO this cloning is bad, too lazy to fix lifetimes
                let str_id = self.pb.builder.add_string(s.clone());
                self.build_literal(cfg::Literal::String(str_id))
            },
            ast::Term::Ident(var_name) => {
                let var_id = self.pb.proc.lookup_var(self.block.scope, var_name).unwrap();
                // TODO var ty fix
                let loaded = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);
                self.block.ops.push(cfg::Op::Load(loaded, var_id));
                loaded
            },
            ast::Term::Call(name, args) => {
                let res = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);

                let arg_exprs: Vec<_> = args.iter().map(|expr| self.build_expr(expr)).collect();

                self.block.ops.push(cfg::Op::Call(res, name.clone(), arg_exprs));

                res
            },
            ast::Term::InterpString(ls, es_pairs) => {
                // TODO fix postfix formatting: text("hello []", 2) => "hello 2"
                // TODO more efficient building, repeated concat bad
                // TODO this cloning is bad, too lazy to fix lifetimes
                let lit = cfg::Literal::String(self.pb.builder.add_string(ls.clone()));
                let mut built = self.build_literal(lit);

                for (o_expr, sep) in es_pairs.iter() {
                    let expr = o_expr.as_ref().expect("postfix formatting not supported currently");
                    let expr_l = self.build_expr(expr);
                    // TODO this is a string, but we treat it as an Any for now
                    let expr_cast_l = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);
                    self.block.ops.push(cfg::Op::Cast(expr_cast_l, expr_l, ty::Primitive::String));

                    // TODO this is a string, but we treat it as an Any for now
                    let new_built = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);
                    self.block.ops.push(cfg::Op::Binary(new_built, BinaryOp::Add, built, expr_cast_l));
                    built = new_built;

                    if sep.len() > 0 {
                        // TODO this is a string, but we treat it as an Any for now
                        let new_built = self.pb.proc.add_local(self.block.scope, ty::Complex::Any, None, false);
                        // TODO this cloning is bad, too lazy to fix lifetimes
                        let lit = cfg::Literal::String(self.pb.builder.add_string(sep.clone()));
                        let lit_l = self.build_literal(lit);
                        self.block.ops.push(cfg::Op::Binary(new_built, BinaryOp::Add, built, lit_l));
                        built = new_built
                    }
                }

                built
            },
            _ => unimplemented!("{:?}", term),
        }
    }
}
