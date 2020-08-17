use super::context::Context;
use inkwell::values::*;
use inkwell::basic_block::BasicBlock;
use caer_types::layout;
use super::value::{StackValue, SSAValue, BVEWrapper};
use super::prog::{ProgEmit, Intrinsic};
use index_vec::IndexVec;
use caer_ir::cfg::*;
use caer_types::ty::{self, Ty};
use caer_ir::id::{VarId, LocalId, BlockId};
use caer_types::op;

#[derive(Debug)]
pub struct ProcEmit<'a, 'p, 'ctx> {
    pub ctx: &'a Context<'a, 'ctx>,
    pub emit: &'a mut ProgEmit<'p, 'ctx>,
    pub var_allocs: IndexVec<VarId, StackValue<'ctx>>,
    pub locals: IndexVec<LocalId, StackValue<'ctx>>,
    local_assigned: IndexVec<LocalId, bool>,
    pub blocks: IndexVec<BlockId, BasicBlock<'ctx>>,
    pub func: FunctionValue<'ctx>,
    pub proc: &'a Proc,
}

impl<'a, 'p, 'ctx> ProcEmit<'a, 'p, 'ctx> {
    pub fn emit(ctx: &'a Context<'p, 'ctx>, emit: &'a mut ProgEmit<'p, 'ctx>, proc: &'a Proc, func: FunctionValue<'ctx>) {
        let mut pe = Self {
            ctx,
            emit,
            var_allocs: IndexVec::new(),
            locals: IndexVec::new(),
            local_assigned: IndexVec::new(),
            blocks: IndexVec::new(),
            func,
            proc,
        };
        pe.emit_proc();
    }

    fn build_val_alloca(&self, ty: &ty::Complex) -> PointerValue<'ctx> {
        match ty {
            ty::Complex::Primitive(prim) => {
                match prim {
                    ty::Primitive::Float => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.f32_type(), ""),
                    ty::Primitive::String => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.i64_type(), ""),
                    ty::Primitive::Ref(_) => self.ctx.builder.build_alloca(self.ctx.rt.ty.datum_common_type_ptr, ""),
                    ty::Primitive::Null => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.i64_type(), ""),
                    _ => unimplemented!("unhandled prim: {:?}", prim),
                }
            },
            ty::Complex::Any | ty::Complex::OneOf(_) => {
                self.ctx.builder.build_alloca(self.ctx.rt.ty.val_type, "")
            },
            _ => unimplemented!("unhandled ty: {:?}", ty),
        }
    }

    fn emit_entry_block(&mut self) -> BasicBlock<'a> {
        let block = self.ctx.llvm_ctx.append_basic_block(self.func, "entry");
        self.ctx.builder.position_at_end(block);

        for (i, local) in self.proc.locals.iter().enumerate() {
            let local_alloca = self.build_val_alloca(&local.ty);
            local_alloca.set_name(&format!("local_{}", i));
            self.locals.push(StackValue::new(Some(local_alloca), local.ty.clone()));
            self.local_assigned.push(false);
        }

        for var in self.proc.vars.iter() {
            let name = &if var.name.index() != 0 {
                format!("var_{}", self.emit.env.string_table.get(var.name))
            } else {
                format!("ret_var")
            };

            let alloca = self.build_val_alloca(&var.ty);
            alloca.set_name(name);
            self.var_allocs.push(StackValue::new(Some(alloca), var.ty.clone()));
        }

        let param_locals_arr_ty = self.ctx.rt.ty.val_type.ptr_type(inkwell::AddressSpace::Generic).array_type(self.proc.params.len() as u32);
        let mut param_locals_arr = param_locals_arr_ty.const_zero();

        for (i, var_id) in self.proc.params.iter().enumerate() {
            let alloc = &self.var_allocs[*var_id];
            // TODO: handle keyword args
            param_locals_arr = self.ctx.builder.build_insert_value(param_locals_arr, alloc.val.unwrap(), i as u32, "param_locals_arr").unwrap().into_array_value();
        }

        let param_locals_alloca = self.ctx.builder.build_alloca(param_locals_arr_ty, "param_locals_alloca");
        self.ctx.builder.build_store(param_locals_alloca, param_locals_arr);

        let cz = self.ctx.llvm_ctx.i32_type().const_zero();
        let param_locals_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(param_locals_alloca, &[cz, cz], "param_locals_ptr") };
        let argpack_local = self.func.get_params()[0];

        self.build_call(self.ctx.rt.rt_arg_pack_unpack_into, &[argpack_local.into(), param_locals_ptr.into(), self.ctx.llvm_ctx.i64_type().const_int(self.proc.id.index() as u64, false).into(), self.emit.rt_global.into()]);

        block
    }

    fn finalize_entry_block(&self, entry: BasicBlock) {
        self.ctx.builder.position_at_end(entry);
        self.ctx.builder.build_unconditional_branch(self.blocks[BlockId::new(0)]);
    }

    pub fn emit_proc(&mut self) {
        let entry_bb = self.emit_entry_block();

        for cfg_block in self.proc.blocks.iter() {
            let block = self.ctx.llvm_ctx.append_basic_block(self.func, &format!("s{}b{}", cfg_block.scope.index(), cfg_block.id.index()));
            self.blocks.push(block);
        }

        for (id, block) in self.proc.blocks.iter_enumerated() {
            let ll_block = self.blocks[id];
            self.ctx.builder.position_at_end(ll_block);
            self.emit_block(block);
        }

        self.finalize_entry_block(entry_bb);
    }

    fn assign_literal(&mut self, lit: &Literal, local_id: LocalId) {
        let lit_ty = lit.get_ty();
        let val: BasicValueEnum = match lit {
            Literal::Num(x) => self.ctx.llvm_ctx.f32_type().const_float(*x as f64).into(),
            Literal::String(id) => self.ctx.llvm_ctx.i64_type().const_int(id.index() as u64, false).into(),
            Literal::Null => self.ctx.llvm_ctx.i64_type().const_zero().into(),
            _ => unimplemented!("{:?}", lit),
        };

        self.set_local(local_id, &SSAValue::new(val, lit_ty));
    }

    // TODO: refactor out as_any, bad.
    fn get_local(&self, local: LocalId, as_any: bool) -> SSAValue<'ctx> {
        match as_any {
            true => self.conv_val(&self.get_local(local, false), &ty::Complex::Any),
            false => self.load_val(&self.locals[local]),
        }
    }

    /// Shortcut method to get an Any llval for a local
    fn get_local_any(&self, local: LocalId) -> SSAValue<'ctx> {
        self.get_local(local, true)
    }

    fn set_local(&mut self, local_id: LocalId, val: &SSAValue<'ctx>) {
        assert!(!self.local_assigned[local_id]);
        let stackptr = self.locals[local_id].val.unwrap();
        let lts_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.start", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lts_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.locals[local_id].ty.get_store_size(), false).into(), stackptr.into()]);
        self.store_val(val, &self.locals[local_id]);
        self.local_assigned[local_id] = true;
    }

    fn local_die(&self, local_id: LocalId) {
        let stackptr = self.locals[local_id].val.unwrap();
        let lte_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.end", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lte_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.locals[local_id].ty.get_store_size(), false).into(), stackptr.into()]);
    }

    fn var_die(&self, var_id: VarId) {
        let stackptr = self.var_allocs[var_id].val.unwrap();
        let lte_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.end", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lte_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.var_allocs[var_id].ty.get_store_size(), false).into(), stackptr.into()]);
    }

    fn load_var(&self, var_id: VarId) -> SSAValue<'ctx> {
        self.load_val(&self.var_allocs[var_id])
    }

    fn store_var(&self, var_id: VarId, val: &SSAValue<'ctx>) {
        self.store_val(val, &self.var_allocs[var_id]);
    }

    fn load_val(&self, stack_val: &StackValue<'ctx>) -> SSAValue<'ctx> {
        let val = self.ctx.builder.build_load(stack_val.val.unwrap(), "");
        SSAValue::new(val, stack_val.ty.clone())
    }

    fn store_val(&self, ssa_val: &SSAValue<'ctx>, stack_val: &StackValue<'ctx>) {
        let conv = self.conv_val(ssa_val, &stack_val.ty);
        self.ctx.builder.build_store(stack_val.val.unwrap(), conv.val);
    }

    fn conv_val(&self, ssa_val: &SSAValue<'ctx>, to_ty: &ty::Complex) -> SSAValue<'ctx> {
        let from_ty = &ssa_val.ty;
        // TODO revisit this as types expand, some assumptions are shaky
        // the panics here indicate an error in type unification, not a user error
        if from_ty == to_ty || (from_ty.is_any() && to_ty.is_any()) {
            // new type has the same memory layout, just return the old val
            ssa_val.clone()
        } else if from_ty.is_any() {
            panic!("attempted to convert Any val with ty {:?} to non-Any ty {:?}", from_ty, to_ty)
        } else if let Some(from_prim) = from_ty.as_primitive() {
            if let Some(to_prim) = to_ty.as_primitive() {
                // TODO: allow upcasting
                // useless check, just for code clarity
                if from_prim != to_prim {
                    panic!("primitive val (ty {:?}) does not match primitive local (ty {:?})", from_ty, to_ty);
                }
            }
            if to_ty.is_any() {
                let (disc, llval) = match from_prim {
                    ty::Primitive::Null => {
                        let null_val = self.ctx.llvm_ctx.i64_type().const_zero();
                        // TODO: less magic for disc vals
                        (0, null_val)
                    },
                    ty::Primitive::Float => {
                        // TODO fix this, this is mega bad, won't work on big endian systems
                        let val_as_i32 = self.ctx.builder.build_bitcast(ssa_val.val, self.ctx.llvm_ctx.i32_type(), "pack_val");
                        let val_as_i64: IntValue = self.ctx.builder.build_int_z_extend(val_as_i32.into_int_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");
                        (1, val_as_i64)
                    },
                    ty::Primitive::String => {
                        let val_as_int = self.ctx.builder.build_bitcast(ssa_val.val, self.ctx.llvm_ctx.i64_type(), "pack_val").into_int_value();
                        (2, val_as_int)
                    },
                    ty::Primitive::Ref(_) => {
                        let val_as_int: IntValue = self.ctx.builder.build_ptr_to_int(ssa_val.val.into_pointer_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");
                        (3, val_as_int)
                    },
                    _ => unimplemented!(),
                };

                let mut local_struct = self.ctx.rt.ty.val_type.const_zero();
                local_struct = self.ctx.builder.build_insert_value(local_struct, self.ctx.llvm_ctx.i32_type().const_int(disc, false), 0, "").unwrap().into_struct_value();
                local_struct = self.ctx.builder.build_insert_value(local_struct, llval, 1, "").unwrap().into_struct_value();

                SSAValue::new(local_struct.into(), to_ty.clone())
            } else {
                panic!("primitive val (ty {:?}) does not fit in local (ty {:?})", from_ty, to_ty);
            }
        } else {
            unimplemented!("unimplemented conv case: {:?} -> {:?}", from_ty, to_ty);
        }
    }

    fn build_call_internal<F>(&self, func: F, args: &[BVEWrapper<'ctx>]) -> Option<BasicValueEnum<'ctx>> where F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>> {
        let args_ll: Vec<_> = args.iter().map(|arg| arg.bve).collect();
        self.ctx.builder.build_call(func, &args_ll, "").try_as_basic_value().left()
    }

    fn build_call<F>(&self, func: F, args: &[BVEWrapper<'ctx>]) -> Option<BasicValueEnum<'ctx>> where F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>> {
        /*let fptr = match func.into() {
            either::Either::Left(func_val) => func_val.as_global_value().as_pointer_value(),
            either::Either::Right(pointer_val) => pointer_val,
        };*/

        //self.build_call_statepoint(fptr, args)
        self.build_call_internal(func, &args)
    }

    fn build_call_statepoint(&self, func: PointerValue<'ctx>, args: &[BasicValueEnum<'ctx>]) -> Option<BasicValueEnum<'ctx>> {
        let sp_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.experimental.gc.statepoint", &[func.get_type().into()]).unwrap()
        };

        let mut sp_args = Vec::with_capacity(7 + args.len());
        sp_args.extend([
            self.ctx.llvm_ctx.i64_type().const_zero().into(), // id
            self.ctx.llvm_ctx.i32_type().const_zero().into(), // # patch bytes
            func.into(), // func
            self.ctx.llvm_ctx.i32_type().const_int(args.len() as u64, false).into(), // # call args
            self.ctx.llvm_ctx.i32_type().const_zero().into(), // flags
        ].iter());
        sp_args.extend(args.iter().copied());
        sp_args.extend([
            self.ctx.llvm_ctx.i32_type().const_zero().into(), // # transition args
            self.ctx.llvm_ctx.i32_type().const_zero().into(), // # deopt args
        ].iter());

        let statepoint_token = self.ctx.builder.build_call(sp_intrinsic, &sp_args, "").try_as_basic_value().left().unwrap();

        let return_type = func.get_type().get_element_type().into_function_type().get_return_type();
        if let Some(return_type) = return_type {
            let result_intrinsic =  unsafe {
                self.ctx.module.get_intrinsic("llvm.experimental.gc.result", &[return_type.into()]).unwrap()
            };
            self.ctx.builder.build_call(result_intrinsic, &[statepoint_token], "").try_as_basic_value().left()
        } else {
            None
        }
    }

    fn build_call_intrinsic(&mut self, intrinsic: Intrinsic, args: &[BVEWrapper<'ctx>]) -> Option<BasicValueEnum<'ctx>> {
        let func = self.emit.get_intrinsic(intrinsic);
        self.build_call_internal(func, args)
    }

    fn build_call_catching<F>(&self, block: &Block, func: F, args: &[BVEWrapper<'ctx>]) -> Option<BasicValueEnum<'ctx>>  where F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>> {
        if let Some(pad) = self.proc.scopes[block.scope].landingpad {
            let args_ll: Vec<_> = args.iter().map(|arg| arg.bve).collect();
            let continuation = self.ctx.llvm_ctx.append_basic_block(self.func, "");
            let res = self.ctx.builder.build_invoke(func, &args_ll, continuation, self.blocks[pad], "").try_as_basic_value().left();
            self.ctx.builder.position_at_end(continuation);
            res
        } else {
            self.build_call_internal(func, args)
        }
    }

    fn emit_block(&mut self, block: &Block) {
        for (i, op) in block.ops.iter().enumerate() {
            match op {
                Op::Noop => {},

                Op::Literal(id, literal) => {
                    self.assign_literal(literal, *id);
                },

                Op::MkVar(_) => {
                    // nothing now - can we remove this?
                }

                Op::Load(local, var) => {
                    let copy = self.load_var(*var);
                    self.set_local(*local, &copy);
                    if copy.ty.needs_destructor() {
                        let copy_any = self.conv_val(&copy, &ty::Complex::Any);
                        self.build_call(self.ctx.rt.rt_val_cloned, &[copy_any.into()]);
                    }
                },

                Op::Store(var, local) => {
                    let var_ty = &self.proc.vars[*var].ty;
                    if var_ty.needs_destructor() {
                        let val = self.load_var(*var);
                        let val_any = self.conv_val(&val, &ty::Complex::Any);
                        self.build_call(self.ctx.rt.rt_val_drop, &[val_any.into()]);
                    }

                    let store_val = self.get_local(*local, false);
                    if store_val.ty.needs_destructor() {
                        let store_val_any = self.conv_val(&store_val, &ty::Complex::Any);
                        self.build_call(self.ctx.rt.rt_val_cloned, &[(&store_val_any).into()],);
                    }
                    self.store_var(*var, &store_val);
                },

                Op::Put(id) => {
                    let norm = self.get_local_any(*id);

                    self.build_call_catching(block, self.ctx.rt.rt_val_print, &[norm.into(), self.emit.rt_global.into()]);
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.get_local_any(*lhs);
                    let rhs_ref = self.get_local_any(*rhs);
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false);

                    let res = self.build_call_catching(block, self.ctx.rt.rt_val_binary_op, &[self.emit.rt_global.into(), op_var.into(), lhs_ref.into(), rhs_ref.into()]).unwrap();
                    self.set_local(*id, &SSAValue::new(res, ty::Complex::Any));
                },

                Op::HardBinary(id, op, lhs, rhs) => {
                    let lhs_val = self.get_local(*lhs, false);
                    let rhs_val = self.get_local(*rhs, false);
                    let res = self.emit_binary(block, *op, &lhs_val, &rhs_val);
                    self.set_local(*id, &res);
                },

                Op::Call(id, name, args) => {
                    let argpack_ptr = self.build_argpack(None, &args);

                    // TODO: better proc lookup, consider src
                    let func = self.emit.lookup_global_proc(*name);
                    let res_val = self.build_call_catching(block, func, &[argpack_ptr.into(), self.emit.rt_global.into()]).unwrap();
                    let val = SSAValue::new(res_val, ty::Complex::Any);
                    self.set_local(*id, &val);
                },

                Op::Cast(dst, src, ty) => {
                    // TODO: impl eq for prim<->complex
                    assert!(self.proc.locals[*dst].ty == (*ty).into());
                    if *ty != ty::Primitive::String {
                        unimplemented!("can only cast to string, not {:?}", ty);
                    }

                    let src_llval = self.get_local_any(*src);
                    let res_llval = self.build_call_catching(block, self.ctx.rt.rt_val_cast_string_val, &[src_llval.into(), self.emit.rt_global.into()]).unwrap();
                    let res_val = SSAValue::new(res_llval, (*ty).into());
                    self.set_local(*dst, &res_val);
                },

                Op::AllocDatum(dst, ty_id) => {
                    // TODO: impl eq for prim<->complex
                    // TODO: assert actual type once inference pops it
                    assert!(self.proc.locals[*dst].ty == ty::Primitive::Ref(None).into());
                    let datum_ptr = self.build_call_catching(block, self.ctx.rt.rt_runtime_alloc_datum, &[
                        self.emit.rt_global.into(),
                        self.ctx.llvm_ctx.i32_type().const_int(ty_id.index() as u64, false).into(),
                    ]).unwrap();
                    let ref_val = SSAValue::new(datum_ptr, ty::Primitive::Ref(None).into());
                    self.set_local(*dst, &ref_val);
                },

                Op::DatumLoadVar(dst, src, var_id) => {
                    let val = self.get_local_any(*src).val.into_struct_value();
                    let ref_ptr = self.build_extract_ref_ptr(val);
                    let var_get_ptr = self.build_vtable_lookup(block, ref_ptr, layout::VTABLE_VAR_GET_FIELD_OFFSET).into_pointer_value();

                    // TODO: drop/cloned

                    let var_val = self.build_call_catching(block, var_get_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.index() as u64, false).into(),
                    ]).unwrap();
                    self.set_local(*dst, &SSAValue::new(var_val, ty::Complex::Any));
                },

                Op::DatumStoreVar(dst, var_id, src) => {
                    let dst_val = self.get_local_any(*dst).val.into_struct_value();
                    let ref_ptr = self.build_extract_ref_ptr(dst_val);
                    let var_set_ptr = self.build_vtable_lookup(block, ref_ptr, layout::VTABLE_VAR_SET_FIELD_OFFSET).into_pointer_value();

                    let src_val = self.get_local_any(*src);
                    self.build_call_catching(block, var_set_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.index() as u64, false).into(),
                        src_val.into(),
                    ]);
                },

                Op::DatumCallProc(dst, src, proc_name, args) => {
                    let argpack_ptr = self.build_argpack(Some(*src), &args);
                    let src_val = self.get_local(*src, false);
                    let src_val_any = self.conv_val(&src_val, &ty::Complex::Any);
                    match src_val.ty {
                        ty::Complex::Primitive(ty::Primitive::Ref(_)) => {
                            let ref_ptr = self.build_extract_ref_ptr(src_val_any.val.into_struct_value());
                            let proc_lookup_ptr = self.build_vtable_lookup(block, ref_ptr, layout::VTABLE_PROC_LOOKUP_FIELD_OFFSET).into_pointer_value();

                            let proc_ptr = self.build_call_catching(block, proc_lookup_ptr, &[
                                self.ctx.llvm_ctx.i64_type().const_int(proc_name.index() as u64, false).into(),
                                self.emit.rt_global.into(),
                            ]).unwrap().into_pointer_value();

                            let res_val = self.build_call_catching(block, proc_ptr, &[
                                argpack_ptr.into(),
                                self.emit.rt_global.into(),
                            ]).unwrap();

                            self.set_local(*dst, &SSAValue::new(res_val, ty::Complex::Any));
                        }
                        _ => {
                            // soft call
                            let res_val = self.build_call_catching(block, self.ctx.rt.rt_val_call_proc, &[
                                src_val_any.into(),
                                self.ctx.llvm_ctx.i64_type().const_int(proc_name.index() as u64, false).into(),
                                argpack_ptr.into(),
                                self.emit.rt_global.into(),
                            ]).unwrap();

                            self.set_local(*dst, &SSAValue::new(res_val, ty::Complex::Any));
                        }
                    }
                },

                Op::Throw(exception_local) => {
                    let exception_val = self.get_local_any(*exception_local);
                    self.build_call_catching(block, self.ctx.rt.rt_throw, &[exception_val.into()]);
                }

                Op::CatchException(maybe_except_var) => {
                    if i != 0 {
                        panic!("CatchException op not at start of block");
                    }

                    let landingpad = self.ctx.builder.build_landingpad(self.ctx.rt.ty.landingpad_type, "lp");
                    landingpad.set_cleanup(true);


                    if let Some(except_var) = maybe_except_var {
                        let exception_container = self.ctx.builder.build_extract_value(landingpad.as_basic_value().into_struct_value(), 0, "").unwrap();
                        let exception = self.build_call(self.ctx.rt.rt_exception_get_val, &[exception_container.into()]).unwrap();
                        let except_val = SSAValue::new(exception, ty::Complex::Any);
                        self.store_var(*except_var, &except_val);
                    }
                },

                //_ => unimplemented!("{:?}", op),
            }
        }

        match &block.terminator {
            Terminator::Return => {
                self.finalize_block(block);
                let ret = self.load_var(VarId::new(0));
                let ret_any = self.conv_val(&ret, &ty::Complex::Any);
                self.ctx.builder.build_return(Some(&ret_any.val));
            },

            Terminator::Jump(id) => {
                self.finalize_block(block);
                self.ctx.builder.build_unconditional_branch(self.blocks[*id]);
            },

            Terminator::Switch { discriminant, branches, default } => {
                let disc_local = &self.proc.locals[*discriminant];

                let disc_int = if let Some(prim_ty) = disc_local.ty.as_primitive() {
                    match prim_ty {
                        ty::Primitive::Null => {
                            self.ctx.llvm_ctx.i32_type().const_int(0, false)
                        },
                        ty::Primitive::Float => {
                            let disc_val = self.get_local(*discriminant, false);
                            self.ctx.builder.build_cast(InstructionOpcode::FPToSI, disc_val.val, self.ctx.llvm_ctx.i32_type(), "disc").into_int_value()
                        },
                        _ => unimplemented!("{:?}", prim_ty),
                    }
                } else {
                    let disc_val = self.get_local_any(*discriminant);
                    self.build_call_catching(block, self.ctx.rt.rt_val_to_switch_disc, &[disc_val.into()]).unwrap().into_int_value()
                };

                self.finalize_block(block);

                // convert branches to use llvm blocks
                let llvm_branches = branches.iter().map(|(lit, target)| {
                    let val = self.ctx.llvm_ctx.i32_type().const_int((*lit).into(), false);
                    (val, self.blocks[*target])
                }).collect::<Vec<_>>();

                self.ctx.builder.build_switch(disc_int, self.blocks[*default], &llvm_branches[..]);
            },

            Terminator::TryCatch { try_block, .. } => {
                self.finalize_block(block);
                self.ctx.builder.build_unconditional_branch(self.blocks[*try_block]);
            }
        }
    }

    fn conv_pred(pred: op::FloatPredicate) -> inkwell::FloatPredicate {
        match pred {
            op::FloatPredicate::OEQ => inkwell::FloatPredicate::OEQ,
            op::FloatPredicate::OGE => inkwell::FloatPredicate::OGE,
            op::FloatPredicate::OGT => inkwell::FloatPredicate::OGT,
            op::FloatPredicate::OLE => inkwell::FloatPredicate::OLE,
            op::FloatPredicate::OLT => inkwell::FloatPredicate::OLT,
            op::FloatPredicate::ONE => inkwell::FloatPredicate::ONE,
            op::FloatPredicate::ORD => inkwell::FloatPredicate::ORD,
            op::FloatPredicate::PredicateFalse => inkwell::FloatPredicate::PredicateFalse,
            op::FloatPredicate::PredicateTrue => inkwell::FloatPredicate::PredicateTrue,
            op::FloatPredicate::UEQ => inkwell::FloatPredicate::UEQ,
            op::FloatPredicate::UGE => inkwell::FloatPredicate::UGE,
            op::FloatPredicate::UGT => inkwell::FloatPredicate::UGT,
            op::FloatPredicate::ULE => inkwell::FloatPredicate::ULE,
            op::FloatPredicate::ULT => inkwell::FloatPredicate::ULT,
            op::FloatPredicate::UNE => inkwell::FloatPredicate::UNE,
            op::FloatPredicate::UNO => inkwell::FloatPredicate::UNO,
        }
    }

    fn emit_binary(&mut self, block: &Block, op: op::HardBinary, lhs: &SSAValue<'ctx>, rhs: &SSAValue<'ctx>) -> SSAValue<'ctx> {
        let res = match op {
            op::HardBinary::StringConcat => {
                self.build_call_catching(block, self.ctx.rt.rt_runtime_concat_strings, &[self.emit.rt_global.into(), lhs.into(), rhs.into()]).unwrap()
            }
            op::HardBinary::FloatAdd => {
                self.ctx.builder.build_float_add(lhs.val.into_float_value(), rhs.val.into_float_value(), "").into()
            }
            op::HardBinary::FloatSub => {
                self.ctx.builder.build_float_sub(lhs.val.into_float_value(), rhs.val.into_float_value(), "").into()
            }
            op::HardBinary::FloatMul => {
                self.ctx.builder.build_float_mul(lhs.val.into_float_value(), rhs.val.into_float_value(), "").into()
            }
            op::HardBinary::FloatDiv => {
                self.ctx.builder.build_float_div(lhs.val.into_float_value(), rhs.val.into_float_value(), "").into()
            }
            op::HardBinary::FloatMod => {
                self.ctx.builder.build_float_rem(lhs.val.into_float_value(), rhs.val.into_float_value(), "").into()
            }
            op::HardBinary::FloatPow => {
                self.build_call_intrinsic(Intrinsic::FPow, &[lhs.into(), rhs.into()]).unwrap()
            }
            op::HardBinary::FloatCmp(pred) => {
                let bool_res = self.ctx.builder.build_float_compare(Self::conv_pred(pred), lhs.val.into_float_value(), rhs.val.into_float_value(), "");
                self.ctx.builder.build_unsigned_int_to_float(bool_res, self.ctx.llvm_ctx.f32_type(), "").into()
            }
            op::HardBinary::FloatBitOp(bitop) => {
                let lhs_i24 = self.float_to_i24(lhs.val.into_float_value());
                let rhs_i24 = self.float_to_i24(rhs.val.into_float_value());
                let res_i24 = match bitop {
                    op::BitOp::And => self.ctx.builder.build_and(lhs_i24, rhs_i24, ""),
                    op::BitOp::Or => self.ctx.builder.build_or(lhs_i24, rhs_i24, ""),
                    op::BitOp::Xor => self.ctx.builder.build_xor(lhs_i24, rhs_i24, ""),
                    op::BitOp::Shl => self.ctx.builder.build_left_shift(lhs_i24, rhs_i24, ""),
                    op::BitOp::Shr => self.ctx.builder.build_right_shift(lhs_i24, rhs_i24, false, ""),
                };
                self.i24_to_float(res_i24).into()
            }
        };
        SSAValue::new(res, op.out_ty())
    }

    // convert an f32 val into a signed i32, saturating at min/max values
    // conversion taken from https://github.com/rust-lang/rust/blob/master/src/librustc_codegen_ssa/mir/rvalue.rs#L883
    // TODO: replace once LLVM gets proper saturating conversion intrinsics. this is bad.
    fn f32_to_i32_sat(&self, f32_val: FloatValue<'ctx>) -> IntValue<'ctx> {
        let i32_type = self.ctx.llvm_ctx.i32_type();
        let f32_type = self.ctx.llvm_ctx.f32_type();

        // bit pattern for -2^31 encoded to f32, rounded towards 0
        let f_min_int = 0xcf000000u32;
        // bit pattern for 2^31-1 encoded to f32, rounded towards 0
        let f_max_int = 0x4effffffu32;

        let f_min_val = self.ctx.builder.build_bitcast(i32_type.const_int(f_min_int.into(), false), f32_type, "").into_float_value();
        let f_max_val = self.ctx.builder.build_bitcast(i32_type.const_int(f_max_int.into(), false), f32_type, "").into_float_value();
        let f_trunc_val = self.ctx.builder.build_float_to_signed_int(f32_val, i32_type, "");

        let i_min_val = i32_type.const_int(i32::MIN as u64, true);
        let i_max_val = i32_type.const_int(i32::MAX as u64, true);

        let less_or_nan = self.ctx.builder.build_float_compare(inkwell::FloatPredicate::ULT, f32_val, f_min_val, "");
        let greater = self.ctx.builder.build_float_compare(inkwell::FloatPredicate::OGT, f32_val, f_max_val, "");
        let is_nan = self.ctx.builder.build_float_compare(inkwell::FloatPredicate::UNO, f32_val, f32_val, "");

        let s0 = self.ctx.builder.build_select(less_or_nan, i_min_val, f_trunc_val, "");
        let s1 = self.ctx.builder.build_select(greater, i_max_val.into(), s0, "");
        self.ctx.builder.build_select(is_nan, i32_type.const_zero().into(), s1, "").into_int_value()
    }

    fn float_to_i24(&self, f32_val: FloatValue<'ctx>) -> IntValue<'ctx> {
        let i24_type = self.ctx.llvm_ctx.custom_width_int_type(24);
        let i32_val = self.f32_to_i32_sat(f32_val);
        self.ctx.builder.build_int_truncate(i32_val, i24_type, "")
    }

    fn i24_to_float(&self, i24_val: IntValue<'ctx>) -> FloatValue<'ctx> {
        self.ctx.builder.build_unsigned_int_to_float(i24_val, self.ctx.llvm_ctx.f32_type(), "")
    }

    fn build_argpack(&self, src: Option<LocalId>, args: &[LocalId]) -> PointerValue<'ctx> {
        let n_val = self.ctx.llvm_ctx.i64_type().const_int(args.len() as u64, false);
        let array_type = self.ctx.rt.ty.val_type.array_type(args.len() as u32);
        let mut argpack_unnamed = array_type.const_zero();
        for (i, arg) in args.iter().enumerate() {
            argpack_unnamed = self.ctx.builder.build_insert_value(argpack_unnamed, self.get_local_any(*arg).val, i as u32, "arg").unwrap().into_array_value();
        }
        let argpack_unnamed_alloca = self.ctx.builder.build_alloca(array_type, "argpack_unnamed_arr");
        self.ctx.builder.build_store(argpack_unnamed_alloca, argpack_unnamed);

        // this GEPpery is bad?
        let cz = self.ctx.llvm_ctx.i32_type().const_zero();
        let argpack_unnamed_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(argpack_unnamed_alloca, &[cz, cz], "argpack_unnamed_ptr") };

        let argpack_alloca = self.ctx.builder.build_alloca(self.ctx.rt.ty.arg_pack_type, "argpack_ptr");
        let mut argpack = self.ctx.rt.ty.arg_pack_type.const_zero().into();
        argpack = self.ctx.builder.build_insert_value(argpack, n_val, 0, "argpack").unwrap();
        argpack = self.ctx.builder.build_insert_value(argpack, argpack_unnamed_ptr, 1, "argpack").unwrap();
        // TODO: named bits
        if let Some(id) = src {
            argpack = self.ctx.builder.build_insert_value(argpack, self.get_local_any(id).val, 4, "argpack").unwrap();
        }

        self.ctx.builder.build_store(argpack_alloca, argpack);

        argpack_alloca
    }

    fn build_extract_ref_ptr(&self, val: StructValue<'ctx>) -> PointerValue<'ctx> {
        // TODO: tycheck we're a ref
        // TODO: use some constant instead of 1
        // TODO: TYAPI
        // TODO: cast val struct instead of int2ptr
        let ref_ptr_int = self.ctx.builder.build_extract_value(val, 1, "ref_ptr_int").unwrap().into_int_value();
        let ref_ptr = self.ctx.builder.build_int_to_ptr(ref_ptr_int, self.ctx.rt.ty.datum_common_type_ptr, "ref_ptr");
        ref_ptr
    }

    fn build_extract_ty_id(&self, datum_ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        let ty_id_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(datum_ptr, &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            self.ctx.llvm_ctx.i32_type().const_int(layout::DATUM_TY_FIELD_OFFSET, false),
        ], "ty_id_ptr") };
        let ty_id = self.ctx.builder.build_load(ty_id_ptr, "ty_id").into_int_value();
        ty_id
    }

    fn build_vtable_lookup_inline(&self, ty_id: IntValue<'ctx>, offset: u64) -> BasicValueEnum<'ctx> {
        let field_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(self.emit.vt_global.as_pointer_value(), &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            ty_id,
            self.ctx.llvm_ctx.i32_type().const_int(offset, false),
        ], &format!("vtable_field_{}_ptr", offset)) };
        let field = self.ctx.builder.build_load(field_ptr, &format!("vtable_field_{}", offset));
        field
    }

    fn build_vtable_lookup(&self, block: &Block, datum_ptr: PointerValue<'ctx>, offset: u64) -> BasicValueEnum<'ctx> {
        let lookup_fn = self.emit.vt_lookup[offset as usize];
        let field = self.build_call_catching(block, lookup_fn, &[datum_ptr.into()]).unwrap();
        field.set_name(&format!("vtable_field_{}", offset));
        field
    }

    fn finalize_block(&self, block: &Block) {
        if block.scope_end {
            for local_id in self.proc.scopes[block.scope].destruct_locals.iter() {
                let local = &self.proc.locals[*local_id];
                if local.ty.needs_destructor() {
                    self.build_call(self.ctx.rt.rt_val_drop, &[self.get_local_any(*local_id).into()]);
                }
                self.local_die(*local_id);
            }
            for var_id in self.proc.scopes[block.scope].destruct_vars.iter() {
                let var = &self.proc.vars[*var_id];
                if var.ty.needs_destructor() {
                    let var_val = self.load_var(*var_id);
                    let var_any = self.conv_val(&var_val, &ty::Complex::Any);
                    self.build_call(self.ctx.rt.rt_val_drop, &[var_any.into()]);
                }
                self.var_die(*var_id);
            }
        }
    }
}
