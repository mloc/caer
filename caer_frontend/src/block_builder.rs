use caer_ir::cfg;
use caer_ir::id::{BlockId, LocalId, VarId};
use caer_types::id::{PathTypeId, StringId, TYPE_ID_ANY, TYPE_ID_STRING};
use caer_types::op::BinaryOp;
use caer_types::ty::{self, RefType};
use dreammaker::ast;

use super::func_builder::FuncBuilder;

// TODO: split this up! somehow

struct EnsureFinalize(bool);

impl Drop for EnsureFinalize {
    fn drop(&mut self) {
        if !self.0 {
            panic!("NOT FINALIZED");
        }
    }
}

// TODO: tidy up the lifetimes dood
pub struct BlockBuilder {
    block: cfg::Block,
    root_block_id: BlockId,
    finalized: EnsureFinalize,
}

impl<'f, 'ot> BlockBuilder {
    pub fn new(block: cfg::Block) -> Self {
        Self {
            root_block_id: block.id,
            block,
            finalized: EnsureFinalize(false),
        }
    }

    pub fn on_block<R>(block: cfg::Block, action: impl FnOnce(&mut BlockBuilder) -> R) -> R {
        let mut bb = BlockBuilder::new(block);
        action(&mut bb)
    }

    pub fn set_terminator(&mut self, terminator: cfg::Terminator) {
        self.block.terminator = terminator;
    }

    pub fn done(self, fb: &mut FuncBuilder<'f, 'ot>) -> BlockId {
        let id = self.root_block_id;
        self.finalize(fb);
        id
    }

    fn finalize(mut self, fb: &mut FuncBuilder<'f, 'ot>) {
        self.finalized.0 = true;
        fb.finalize_block(self.block);
    }

    fn cur_block_done(&mut self, fb: &mut FuncBuilder<'f, 'ot>, replacement: cfg::Block) {
        let mut swp = replacement;
        std::mem::swap(&mut self.block, &mut swp);
        fb.finalize_block(swp);
    }

    pub fn build_stmts(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>,
        stmts: impl Iterator<Item = &'ot ast::Spanned<ast::Statement>>,
    ) {
        for stmt in stmts {
            self.build_stmt(fb, &stmt.elem);
        }
    }

    pub fn build_stmt(&mut self, fb: &mut FuncBuilder<'f, 'ot>, stmt: &'ot ast::Statement) {
        match stmt {
            ast::Statement::Var(v) => {
                let var = self.add_var(fb, &v.var_type, &v.name);
                self.block.push_op(cfg::Op::MkVar(var));

                let name_id = fb.ir.intern_string(&v.name);
                if let Some(expr) = &v.value {
                    self.build_assign(fb, None, name_id, expr);
                } else {
                    let null = self.build_literal(fb, cfg::Literal::Null);
                    let var_id = fb.lookup_var(self.block.scope, name_id);
                    self.block.push_op(cfg::Op::Store(var_id, null));
                }
            },

            ast::Statement::Expr(expr) => match expr {
                ast::Expression::AssignOp { op: _, lhs, rhs } => {
                    let (base, var) = self.build_expr_lhs(fb, lhs.as_ref());
                    self.build_assign(fb, base, var, &*rhs);
                },

                ast::Expression::BinaryOp { op, lhs: _, rhs } => {
                    let res = self.build_expr(fb, rhs);
                    match op {
                        ast::BinaryOp::LShift => {
                            self.block.push_op(cfg::Op::Put(res));
                        },
                        _ => {
                            println!("uh oh {:#?}", expr);
                            unimplemented!("{:?}", op);
                        },
                    };
                },

                expr => {
                    self.build_expr(fb, expr);
                },
            },

            ast::Statement::Return(val) => {
                if let Some(val) = val {
                    let val_expr = self.build_expr(fb, val);
                    // TODO: no magic number for ret var
                    self.block.push_op(cfg::Op::Store(VarId::new(0), val_expr));
                    // TODO make sure we check the rest of the ops somehow?
                    // create an unreachable block and continue? need a more robust builder
                    return;
                }
            },

            ast::Statement::If { arms, else_arm } => {
                let if_end = fb.new_block(self.block.scope);

                // TODO clean up this scope creation with better builder
                for (condition, body) in arms.iter() {
                    let cond_scope = fb.new_scope(self.block.scope);
                    let mut cond_bb = BlockBuilder::new(fb.new_block(cond_scope));
                    cond_bb.block.scope_end = true;

                    let cond_expr = cond_bb.build_expr(fb, &condition.elem);

                    let (body_block_id, _) =
                        fb.build_block(&body[..], Some(self.block.scope), Some(if_end.id));

                    let continuation = fb.new_block(self.block.scope);

                    self.block.terminator = cfg::Terminator::Jump(cond_bb.root_block_id);
                    cond_bb.block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, continuation.id)],
                        default: body_block_id,
                    };

                    cond_bb.finalize(fb);
                    self.cur_block_done(fb, continuation);
                }

                let mut next = if_end.id;
                if let Some(body) = else_arm {
                    next = fb
                        .build_block(&body[..], Some(self.block.scope), Some(if_end.id))
                        .0;
                }

                // adds a redundant jump, but simplifies code.
                // will be optimized out anyway
                self.block.terminator = cfg::Terminator::Jump(next);

                self.cur_block_done(fb, if_end);
            },

            ast::Statement::While { condition, block } => {
                let cond_scope = fb.new_scope(self.block.scope);
                let mut cond_bb = BlockBuilder::new(fb.new_block(cond_scope));
                cond_bb.block.scope_end = true;
                let while_end = fb.new_block(self.block.scope);

                let (body_block_id, _) = fb.build_block(
                    &block[..],
                    Some(self.block.scope),
                    Some(cond_bb.root_block_id),
                );

                let cond_expr = cond_bb.build_expr(fb, condition);

                cond_bb.block.terminator = cfg::Terminator::Switch {
                    discriminant: cond_expr,
                    branches: vec![(0, while_end.id)],
                    default: body_block_id,
                };

                self.block.terminator = cfg::Terminator::Jump(cond_bb.root_block_id);

                cond_bb.finalize(fb);
                self.cur_block_done(fb, while_end);
            },

            ast::Statement::ForLoop {
                init,
                test,
                inc,
                block,
            } => {
                let loop_scope = fb.new_scope(self.block.scope);
                let mut init_bb = BlockBuilder::new(fb.new_block(loop_scope));
                let mut cond_bb = BlockBuilder::new(fb.new_block(loop_scope));
                let mut inc_bb = BlockBuilder::new(fb.new_block(loop_scope));
                let mut exit_block = fb.new_block(loop_scope);
                exit_block.scope_end = true;

                if let Some(ref init_stmt) = init {
                    init_bb.build_stmt(fb, init_stmt);
                }

                let (body_id, _) =
                    fb.build_block(block, Some(loop_scope), Some(inc_bb.root_block_id));

                if let Some(ref test_expr) = test {
                    let cond_expr = cond_bb.build_expr(fb, test_expr);
                    cond_bb.block.terminator = cfg::Terminator::Switch {
                        discriminant: cond_expr,
                        branches: vec![(0, exit_block.id)],
                        default: body_id,
                    };
                } else {
                    cond_bb.block.terminator = cfg::Terminator::Jump(body_id);
                }

                if let Some(ref inc_stmt) = inc {
                    inc_bb.build_stmt(fb, inc_stmt);
                }

                init_bb.block.terminator = cfg::Terminator::Jump(cond_bb.root_block_id);
                inc_bb.block.terminator = cfg::Terminator::Jump(cond_bb.root_block_id);

                let continuation = fb.new_block(self.block.scope);
                self.block.terminator = cfg::Terminator::Jump(init_bb.root_block_id);
                exit_block.terminator = cfg::Terminator::Jump(continuation.id);

                init_bb.finalize(fb);
                cond_bb.finalize(fb);
                inc_bb.finalize(fb);
                fb.finalize_block(exit_block);
                self.cur_block_done(fb, continuation);
            },

            ast::Statement::TryCatch {
                try_block,
                catch_param,
                catch_block,
            } => {
                let continuation = fb.new_block(self.block.scope);
                let (try_id, try_scope) =
                    fb.build_block(try_block, Some(self.block.scope), Some(continuation.id));

                let mut catch_bb = fb.builder_within_scope(self.block.scope);
                let catch_var;
                if let Some((var_path, var_name)) = catch_param {
                    let var_id = catch_bb.add_var(fb, var_path, var_name);
                    catch_var = Some(var_id)
                } else {
                    catch_var = None
                }
                catch_bb.block.push_op(cfg::Op::CatchException(catch_var));
                catch_bb.build_stmts(fb, catch_block.iter());
                catch_bb.block.terminator = cfg::Terminator::Jump(continuation.id);

                fb.set_landingpad(try_scope, catch_bb.root_block_id);

                self.block.terminator = cfg::Terminator::TryCatch {
                    try_block: try_id,
                    catch_block: catch_bb.root_block_id,
                };

                catch_bb.finalize(fb);
                self.cur_block_done(fb, continuation);
            },

            ast::Statement::Throw(throw_expr) => {
                let throw_val = self.build_expr(fb, throw_expr);
                self.block.push_op(cfg::Op::Throw(throw_val));
            },

            ast::Statement::Spawn { delay, block } => {
                /*let closure = ClosureEnvironment::new(fb, self.block.scope);
                let closure_builder = FuncBuilder::for_closure(env, objtree, closure);

                // TODO: dedup
                let func = closure_builder.finalize();
                let id = func.id;
                fb.env.add_func(func);
                self.block.push_op(cfg::Op::Spawn(id));*/
                let slot = fb.add_closure_slot(self.block.scope, block);
                let delay_val = delay.as_ref().map(|d| self.build_expr(fb, d));
                self.block.push_op(cfg::Op::Spawn(slot, delay_val));
                //return Some((slot, block));
            },

            _ => unimplemented!(),
        }
    }

    fn add_var(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, var_type: &ast::VarType, var_name: &str,
    ) -> VarId {
        // TODO: care about var flags?
        let name_id = fb.ir.intern_string(var_name);
        let var = fb.add_var(self.block.scope, name_id);

        if !var_type.type_path.is_empty() {
            // TODO: handle list types
            let var_ty = match fb.objtree.resolve_path(&var_type.type_path) {
                Some(ty) => ty,
                None => {
                    // TODO: ERRH(C)
                    panic!("no such type {:?}", var_type.type_path);
                },
            };

            // TODO: encapsulate type_tree
            let dty_id = fb.objtree.lookup_type(var_ty).unwrap();
            fb.set_var_dty(var, dty_id);
        }

        var
    }

    fn build_expr_base(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, term: &ast::Spanned<ast::Term>,
        unary_ops: &[ast::UnaryOp], follow: &[ast::Spanned<ast::Follow>],
    ) -> LocalId {
        let mut local = self.build_term(fb, &term.elem);
        assert!(unary_ops.is_empty());

        for follow_span in follow {
            local = self.build_follow(fb, local, follow_span);
        }

        local
    }

    fn build_follow(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, local: LocalId,
        follow: &ast::Spanned<ast::Follow>,
    ) -> LocalId {
        match &follow.elem {
            ast::Follow::Field(kind, field) => {
                let field_id = fb.ir.intern_string(field);
                self.build_datum_load(fb, local, field_id, *kind)
            },
            ast::Follow::Call(kind, proc_name, args) => {
                let proc_name_id = fb.ir.intern_string(proc_name);
                let arg_exprs: Vec<_> = args.iter().map(|expr| self.build_expr(fb, expr)).collect();
                self.build_datum_call(fb, local, proc_name_id, arg_exprs, *kind)
            },
            f => unimplemented!("follow: {:?}", f),
        }
    }

    fn build_expr_lhs(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, expr: &ast::Expression,
    ) -> (Option<LocalId>, StringId) {
        match expr {
            ast::Expression::Base {
                unary,
                term,
                follow,
            } => {
                assert!(unary.is_empty());

                if follow.is_empty() {
                    match &term.elem {
                        ast::Term::Ident(ref s) => return (None, fb.ir.intern_string(s)),
                        t => panic!("singleton lhs term must be an ident, not {:?}", t),
                    }
                };

                let mut base = self.build_term(fb, &term.elem);

                let (follow_last, follow_rest) = follow.split_last().unwrap();

                for follow_span in follow_rest.iter() {
                    base = self.build_follow(fb, base, follow_span);
                }

                match &follow_last.elem {
                    ast::Follow::Field(kind, field) => {
                        let field_id = fb.ir.intern_string(field);
                        self.validate_datum_index(fb, base, field_id, *kind);
                        (Some(base), field_id)
                    },
                    // TODO: handle indexes
                    f => panic!("bad follow on lhs: {:?}", f),
                }
            },

            _ => unimplemented!(),
        }
    }

    fn build_assign(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, base: Option<LocalId>, var: StringId,
        expr: &ast::Expression,
    ) {
        let asg_expr = self.build_expr(fb, expr);
        match base {
            None => {
                let var_id = fb.lookup_var(self.block.scope, var);
                self.block.push_op(cfg::Op::Store(var_id, asg_expr));
            },
            Some(datum_id) => {
                self.block
                    .push_op(cfg::Op::DatumStoreVar(datum_id, var, asg_expr));
            },
        }
    }

    fn build_var_load(&mut self, fb: &mut FuncBuilder<'f, 'ot>, var: VarId) -> LocalId {
        // TODO var ty fix
        let holder = fb.add_local(self.block.scope, ty::Type::Any);
        if let Some(assoc) = fb.func.vars[var].assoc_dty {
            fb.func.set_assoc_dty(holder, assoc);
        }
        self.block.push_op(cfg::Op::Load(holder, var));
        holder
    }

    // TODO: ERRH(fe)
    fn build_datum_load(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, datum_local: LocalId, var: StringId,
        index_kind: ast::IndexKind,
    ) -> LocalId {
        let field_dty_id = self.validate_datum_index(fb, datum_local, var, index_kind);
        // TODO: reconsider the scope here
        let loaded_local = fb.func.add_local(self.block.scope, TYPE_ID_ANY);
        if let Some(dty_id) = field_dty_id {
            fb.func.set_assoc_dty(loaded_local, dty_id);
        }
        self.block
            .push_op(cfg::Op::DatumLoadVar(loaded_local, datum_local, var));
        loaded_local
    }

    fn build_datum_call(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, datum_local: LocalId, proc_name: StringId,
        args: Vec<LocalId>, index_kind: ast::IndexKind,
    ) -> LocalId {
        match index_kind {
            ast::IndexKind::Colon | ast::IndexKind::SafeColon => {},
            ast::IndexKind::Dot | ast::IndexKind::SafeDot => {
                let dty_id = match fb.func.get_assoc_dty(datum_local) {
                    Some(id) => id,
                    None => panic!(
                        "no associated dty on local {:?}, but attempted dot-dereference for proc",
                        datum_local
                    ),
                };

                // TODO: encap tt
                let dty = &fb.ir.type_tree.get_pty(dty_id).unwrap();
                if !dty.proc_lookup.contains_key(&proc_name) {
                    panic!(
                        "type {} has no proc {}",
                        fb.ir.string_table.get(dty.path_str),
                        fb.ir.string_table.get(proc_name)
                    );
                }
            },
        }

        let res_local = fb.func.add_local(self.block.scope, TYPE_ID_ANY);
        self.block.push_op(cfg::Op::DatumCallProc(
            res_local,
            datum_local,
            proc_name,
            args,
        ));
        res_local
    }

    // TODO: ERRH(fe)
    /// Returns the dtype ID of the indexed field, if indexed with a .-like operator
    fn validate_datum_index(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, datum_local: LocalId, var: StringId,
        index_kind: ast::IndexKind,
    ) -> Option<PathTypeId> {
        match index_kind {
            ast::IndexKind::Colon | ast::IndexKind::SafeColon => None,
            ast::IndexKind::Dot | ast::IndexKind::SafeDot => {
                let dty_id = match fb.func.get_assoc_dty(datum_local) {
                    Some(id) => id,
                    None => panic!(
                        "no associated dty on local {:?}, but attempted dot-dereference for var",
                        datum_local
                    ),
                };

                // TODO: encap tt
                let dty = &fb.ir.type_tree.get_pty(dty_id).unwrap();
                if let Some(var_info) = dty.var_lookup.get(&var) {
                    var_info.assoc_dty
                } else {
                    panic!(
                        "type {} has no var {}",
                        fb.ir.string_table.get(dty.path_str),
                        fb.ir.string_table.get(var)
                    );
                }
            },
        }
    }

    fn build_expr(&mut self, fb: &mut FuncBuilder<'f, 'ot>, expr: &ast::Expression) -> LocalId {
        match expr {
            ast::Expression::Base {
                unary,
                term,
                follow,
            } => self.build_expr_base(fb, term, &unary, &follow),

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                if *op == ast::BinaryOp::And || *op == ast::BinaryOp::Or {
                    return self.build_logical_binop(fb, lhs, rhs, op);
                }

                let lhs_expr = self.build_expr(fb, lhs);
                let rhs_expr = self.build_expr(fb, rhs);
                let res_local = fb.add_local(self.block.scope, ty::Type::Any);

                let l_op = match op {
                    ast::BinaryOp::Add => BinaryOp::Add,
                    ast::BinaryOp::Sub => BinaryOp::Sub,
                    ast::BinaryOp::Mul => BinaryOp::Mul,
                    ast::BinaryOp::Div => BinaryOp::Div,
                    ast::BinaryOp::Pow => BinaryOp::Pow,
                    ast::BinaryOp::Mod => BinaryOp::Mod,
                    ast::BinaryOp::Eq => BinaryOp::Eq,
                    ast::BinaryOp::NotEq => BinaryOp::Ne,
                    ast::BinaryOp::Less => BinaryOp::Lt,
                    ast::BinaryOp::Greater => BinaryOp::Gt,
                    ast::BinaryOp::LessEq => BinaryOp::Le,
                    ast::BinaryOp::GreaterEq => BinaryOp::Ge,
                    ast::BinaryOp::Equiv => BinaryOp::Equiv,
                    ast::BinaryOp::NotEquiv => BinaryOp::NotEquiv,
                    ast::BinaryOp::BitAnd => BinaryOp::BitAnd,
                    ast::BinaryOp::BitXor => BinaryOp::BitXor,
                    ast::BinaryOp::BitOr => BinaryOp::BitOr,
                    ast::BinaryOp::LShift => BinaryOp::Shl,
                    ast::BinaryOp::RShift => BinaryOp::Shr,
                    _ => unimplemented!("binary op {:?}", op),
                };

                self.block
                    .push_op(cfg::Op::Binary(res_local, l_op, lhs_expr, rhs_expr));

                res_local
            },

            _ => unimplemented!("{:?}", expr),
        }
    }

    // build a binop with short-circuiting
    fn build_logical_binop(
        &mut self, fb: &mut FuncBuilder<'f, 'ot>, lhs: &ast::Expression, rhs: &ast::Expression,
        op: &ast::BinaryOp,
    ) -> LocalId {
        // TODO: nameless vars
        let res_var_name = fb.ir.intern_string("__logical_binop_res");
        let res_var = fb.add_var(self.block.scope, res_var_name);
        self.block.push_op(cfg::Op::MkVar(res_var));
        let lhs_expr = self.build_expr(fb, lhs);
        self.block.push_op(cfg::Op::Store(res_var, lhs_expr));

        let end_block = fb.new_block(self.block.scope);
        let mut alt_bb = BlockBuilder::new(fb.new_block(self.block.scope));
        let mut wipeout_bb = BlockBuilder::new(fb.new_block(self.block.scope));

        let rhs_expr = alt_bb.build_expr(fb, rhs);
        alt_bb.block.push_op(cfg::Op::Store(res_var, rhs_expr));
        alt_bb.block.terminator = cfg::Terminator::Switch {
            discriminant: rhs_expr,
            branches: vec![(0, wipeout_bb.root_block_id)],
            default: end_block.id,
        };

        let wipeout = wipeout_bb.build_literal(fb, cfg::Literal::Null);
        wipeout_bb.block.push_op(cfg::Op::Store(res_var, wipeout));
        wipeout_bb.block.terminator = cfg::Terminator::Jump(end_block.id);

        let (true_path, false_path) = match *op {
            ast::BinaryOp::And => (alt_bb.root_block_id, wipeout_bb.root_block_id),
            ast::BinaryOp::Or => (end_block.id, alt_bb.root_block_id),
            _ => panic!("build_logical_binop called with non-logical op {:?}", op),
        };

        self.block.terminator = cfg::Terminator::Switch {
            discriminant: lhs_expr,
            branches: vec![(0, false_path)],
            default: true_path,
        };

        alt_bb.finalize(fb);
        wipeout_bb.finalize(fb);
        self.cur_block_done(fb, end_block);

        // oneof ty?
        let res = fb.add_local(self.block.scope, ty::Type::Any);
        self.block.push_op(cfg::Op::Load(res, res_var));
        res
    }

    fn build_literal(&mut self, fb: &mut FuncBuilder<'f, 'ot>, lit: cfg::Literal) -> LocalId {
        let local = fb.add_local(self.block.scope, lit.get_ty());
        self.block.push_op(cfg::Op::Literal(local, lit));
        local
    }

    fn build_term(&mut self, fb: &mut FuncBuilder<'f, 'ot>, term: &ast::Term) -> LocalId {
        match term {
            ast::Term::Expr(e) => self.build_expr(fb, e),
            ast::Term::Null => self.build_literal(fb, cfg::Literal::Null),
            ast::Term::Int(x) => self.build_literal(fb, cfg::Literal::Num(*x as f32)),
            ast::Term::Float(x) => self.build_literal(fb, cfg::Literal::Num(*x)),
            ast::Term::String(s) => {
                // TODO this cloning is bad, too lazy to fix lifetimes
                let str_id = fb.ir.intern_string(s.clone());
                self.build_literal(fb, cfg::Literal::String(str_id))
            },
            ast::Term::Ident(var_name) => {
                let name_id = fb.ir.intern_string(var_name);
                let var_id = fb.lookup_var(self.block.scope, name_id);
                self.build_var_load(fb, var_id)
            },
            ast::Term::Call(name, args) => {
                let res = fb.add_local(self.block.scope, ty::Type::Any);

                let mut arg_exprs = Vec::with_capacity(args.len());
                for expr in args {
                    arg_exprs.push(self.build_expr(fb, expr));
                }

                let name_id = fb.ir.intern_string(name);
                self.block.push_op(cfg::Op::Call(res, name_id, arg_exprs));

                res
            },
            ast::Term::InterpString(ls, es_pairs) => {
                // TODO fix postfix formatting: text("hello []", 2) => "hello 2"
                // TODO more efficient building, repeated concat bad
                let lit = cfg::Literal::String(fb.ir.intern_string(ls));
                let mut built = self.build_literal(fb, lit);

                for (o_expr, sep) in es_pairs.iter() {
                    let expr = o_expr
                        .as_ref()
                        .expect("postfix formatting not supported currently");
                    let expr_l = self.build_expr(fb, expr);
                    let expr_cast_l = fb.add_local(self.block.scope, RefType::String.into());
                    self.block
                        .push_op(cfg::Op::Cast(expr_cast_l, expr_l, TYPE_ID_STRING));

                    // TODO: we know this is a string, but Binary doesn't
                    let new_built = fb.add_local(self.block.scope, ty::Type::Any);
                    self.block.push_op(cfg::Op::Binary(
                        new_built,
                        BinaryOp::Add,
                        built,
                        expr_cast_l,
                    ));
                    built = new_built;

                    if !sep.is_empty() {
                        // this is a soft concat op. type inference will constrain it into the
                        // appropriate hard op
                        let new_built = fb.add_local(self.block.scope, ty::Type::Any);
                        let lit = cfg::Literal::String(fb.ir.intern_string(sep));
                        let lit_l = self.build_literal(fb, lit);
                        self.block
                            .push_op(cfg::Op::Binary(new_built, BinaryOp::Add, built, lit_l));
                        built = new_built
                    }
                }

                built
            },
            ast::Term::New { type_: newty, args } => {
                assert!(args.is_none());

                let ty_id = match newty {
                    ast::NewType::Prefab(pf) => {
                        assert!(pf.vars.is_empty());
                        // TODO: ughhhh, move path resolving into a helper, better encapsulation
                        let pf_node = fb.objtree.navigate_path(&pf.path).unwrap();
                        fb.objtree.lookup_type(pf_node).unwrap()
                    },
                    _ => unimplemented!("new with newty {:?}", newty),
                };

                let ref_local = fb.add_local(self.block.scope, RefType::Exact(ty_id).into());

                self.block.push_op(cfg::Op::AllocDatum(ref_local, ty_id));

                ref_local
            },
            _ => unimplemented!("{:?}", term),
        }
    }
}
