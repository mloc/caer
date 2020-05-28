use super::proc_builder::ProcBuilder;
use crate::ir::cfg;
use crate::ir::id::*;
use dreammaker::ast;
use indexed_vec::Idx;
use caer_runtime::op::BinaryOp;
use caer_runtime::string_table::StringId;
use caer_runtime::type_tree;
use crate::ty;

// these lifetimes are probably extraneous, they're a kludge for speed
pub struct BlockBuilder<'a, 'pb, 'cb, 'ot> {
    pb: &'a mut ProcBuilder<'pb, 'cb, 'ot>,
    block: cfg::Block,
    pub root_block_id: BlockId,
}

impl<'a, 'pb, 'cb, 'ot> BlockBuilder<'a, 'pb, 'cb, 'ot> {
    pub fn new(pb: &'a mut ProcBuilder<'pb, 'cb, 'ot>, scope: ScopeId) -> Self {
        let block = pb.proc.new_block(scope);

        Self {
            pb: pb,
            root_block_id: block.id,
            block: block,
        }
    }

    pub fn done(self) -> cfg::Block {
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


    pub fn build_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Var(v) => {
                // TODO: care about var flags?
                let name_id = self.pb.builder.add_string(&v.name);
                let local = self.pb.add_var(self.block.scope, name_id);

                if !v.var_type.type_path.is_empty() {
                    // TODO: handle list types
                    let var_ty = match self.pb.builder.objtree.type_by_path(&v.var_type.type_path) {
                        Some(ty) => ty,
                        None => {
                            // TODO: ERRH(C)
                            panic!("no such type {:?}", v.var_type.type_path);
                        },
                    };

                    // TODO: encapsulate type_tree
                    let dty_id = self.pb.builder.env.rt_env.type_tree.type_by_node_id[&(var_ty.index().index() as u64)];
                    self.pb.proc.set_assoc_dty(local, dty_id);
                }

                self.push_op(cfg::Op::MkVar(local));

                if let Some(expr) = &v.value {
                    self.build_assign(None, name_id, expr);
                }
            },

            ast::Statement::Expr(expr) => {
                match expr {
                    ast::Expression::AssignOp { op, lhs, rhs } => {
                        let mut base = None;
                        let var = match lhs.as_ref() {
                            ast::Expression::Base { unary, term, follow } => {
                                assert!(unary.len() == 0);

                                let mut final_field = match term.elem {
                                    ast::Term::Ident(ref s) => self.pb.builder.add_string(s),
                                    _ => unimplemented!(),
                                };

                                let mut prev_kind = None;
                                for follow_span in follow {
                                    match &follow_span.elem {
                                        ast::Follow::Field(kind, field) => {
                                            let holder = match base {
                                                None => {
                                                    assert!(prev_kind.is_none());
                                                    let var_id = self.pb.proc.lookup_var(self.block.scope, final_field).unwrap();
                                                    // TODO: this should have a narrower scope.
                                                    self.build_var_load(var_id)
                                                },
                                                Some(id) => {
                                                    self.build_datum_load(id, final_field, prev_kind.unwrap())
                                                },
                                            };

                                            base = Some(holder);
                                            let field_id = self.pb.builder.add_string(field);
                                            final_field = field_id;
                                            prev_kind = Some(*kind);
                                        },
                                        f => unimplemented!("lhs term follow: {:?}", f),
                                    }
                                }

                                if let Some(id) = base {
                                    self.validate_datum_index(id, final_field, prev_kind.unwrap());
                                }

                                final_field
                            },

                            _ => unimplemented!(),
                        };

                        self.build_assign(base, var, &*rhs);
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
                    self.push_op(cfg::Op::Store(LocalId::new(0), val_expr));
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

    fn build_assign(&mut self, base: Option<LocalId>, var: StringId, expr: &ast::Expression) {
        let asg_expr = self.build_expr(expr);
        match base {
            None => {
                let var_id = self.pb.proc.lookup_var(self.block.scope, var).unwrap();
                self.push_op(cfg::Op::Store(var_id, asg_expr));
            },
            Some(datum_id) => {
                self.push_op(cfg::Op::DatumStoreVar(datum_id, var, asg_expr));
            },
        }
    }

    fn build_var_load(&mut self, var_local: LocalId) -> LocalId {
        // TODO var ty fix
        let holder = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);
        if let Some(assoc) = self.pb.proc.get_assoc_dty(var_local) {
            self.pb.proc.set_assoc_dty(holder, assoc);
        }
        self.push_op(cfg::Op::Load(holder, var_local));
        holder
    }

    // TODO: ERRH(C)
    fn build_datum_load(&mut self, datum_local: LocalId, var: StringId, index_kind: ast::IndexKind) -> LocalId {
        let field_dty_id = self.validate_datum_index(datum_local, var, index_kind);
        // TODO: reconsider the scope here
        let loaded_local = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);
        if let Some(dty_id) = field_dty_id {
            self.pb.proc.set_assoc_dty(loaded_local, dty_id);
        }
        self.push_op(cfg::Op::DatumLoadVar(loaded_local, datum_local, var));
        loaded_local
    }

    // TODO: ERRH(C)
    /// Returns the dtype ID of the indexed field, if indexed with a .-like operator
    fn validate_datum_index(&mut self, datum_local: LocalId, var: StringId, index_kind: ast::IndexKind) -> Option<type_tree::TypeId> {
        match index_kind {
            ast::IndexKind::Colon | ast::IndexKind::SafeColon => None,
            ast::IndexKind::Dot | ast::IndexKind::SafeDot => {
                let dty_id = match self.pb.proc.get_assoc_dty(datum_local) {
                    Some(id) => id,
                    None => panic!("NO TYPE"),
                };

                // TODO: encap tt
                let dty = &self.pb.builder.env.rt_env.type_tree.types[dty_id];
                if let Some(var_info) = dty.var_lookup.get(&var) {
                    var_info.assoc_dty
                } else {
                    panic!("type {} has no var {}", self.pb.builder.env.string_table.get(dty.path_str), self.pb.builder.env.string_table.get(var));
                }
            },
        }
    }

    fn build_expr(&mut self, expr: &ast::Expression) -> LocalId {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                let mut local = self.build_term(&term.elem);
                assert!(unary.len() == 0);

                for follow_span in follow {
                    match &follow_span.elem {
                        ast::Follow::Field(kind, field) => {
                            let field_id = self.pb.builder.add_string(field);
                            local = self.build_datum_load(local, field_id, *kind);
                        },
                        f => unimplemented!("follow: {:?}", f),
                    }
                }

                local
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs);
                let rhs_expr = self.build_expr(rhs);
                let local = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);

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

    fn build_literal(&mut self, lit: cfg::Literal) -> LocalId {
        let local = self.pb.proc.add_local(self.block.scope, lit.get_ty());
        self.push_op(cfg::Op::Literal(local, lit));
        local
    }

    fn build_term(&mut self, term: &ast::Term) -> LocalId {
        match term {
            ast::Term::Int(x) => self.build_literal(cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => self.build_literal(cfg::Literal::Num(*x)),
            ast::Term::String(s) => {
                // TODO this cloning is bad, too lazy to fix lifetimes
                let str_id = self.pb.builder.add_string(s.clone());
                self.build_literal(cfg::Literal::String(str_id))
            },
            ast::Term::Ident(var_name) => {
                let name_id = self.pb.builder.add_string(var_name);
                let var_id = self.pb.proc.lookup_var(self.block.scope, name_id).unwrap();
                self.build_var_load(var_id)
            },
            ast::Term::Call(name, args) => {
                let res = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);

                let arg_exprs: Vec<_> = args.iter().map(|expr| self.build_expr(expr)).collect();

                let name_id = self.pb.builder.add_string(name);
                self.push_op(cfg::Op::Call(res, name_id, arg_exprs));

                res
            },
            ast::Term::InterpString(ls, es_pairs) => {
                // TODO fix postfix formatting: text("hello []", 2) => "hello 2"
                // TODO more efficient building, repeated concat bad
                let lit = cfg::Literal::String(self.pb.builder.add_string(ls));
                let mut built = self.build_literal(lit);

                for (o_expr, sep) in es_pairs.iter() {
                    let expr = o_expr.as_ref().expect("postfix formatting not supported currently");
                    let expr_l = self.build_expr(expr);
                    // TODO this is a string, but we treat it as an Any for now
                    let expr_cast_l = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);
                    self.push_op(cfg::Op::Cast(expr_cast_l, expr_l, ty::Primitive::String));

                    // TODO this is a string, but we treat it as an Any for now
                    let new_built = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);
                    self.push_op(cfg::Op::Binary(new_built, BinaryOp::Add, built, expr_cast_l));
                    built = new_built;

                    if sep.len() > 0 {
                        // TODO this is a string, but we treat it as an Any for now
                        let new_built = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);
                        let lit = cfg::Literal::String(self.pb.builder.add_string(sep));
                        let lit_l = self.build_literal(lit);
                        self.push_op(cfg::Op::Binary(new_built, BinaryOp::Add, built, lit_l));
                        built = new_built
                    }
                }

                built
            },
            ast::Term::New { type_: newty, args } => {
                assert!(args.is_none());
                // TODO this is a ref, but we treat it as an Any for now
                let ref_local = self.pb.proc.add_local(self.block.scope, ty::Complex::Any);

                let ty_id = match newty {
                    ast::NewType::Prefab(pf) => {
                        assert!(pf.vars.is_empty());
                        // TODO: ughhhh, move path resolving into a helper, better encapsulation
                        let pf_ty = self.pb.builder.objtree.root().navigate_path(&pf.path).unwrap().ty();
                        let type_id = self.pb.builder.env.rt_env.type_tree.type_by_node_id[&(pf_ty.index().index() as u64)];
                        type_id
                    },
                    _ => unimplemented!("new with newty {:?}", newty),
                };

                self.push_op(cfg::Op::AllocDatum(ref_local, ty_id));

                ref_local
            }
            _ => unimplemented!("{:?}", term),
        }
    }
}
