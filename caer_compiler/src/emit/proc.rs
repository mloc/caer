use std::borrow::Borrow;
use super::context::Context;
use super::value::Value;
use super::prog::ProgEmit;
use index_vec::IndexVec;
use crate::ir::cfg::*;
use crate::ir::id::*;
use crate::ty::{self, Ty};
use caer_runtime::datum;
use caer_runtime::vtable;

#[derive(Debug)]
pub struct ProcEmit<'a, 'ctx> {
    pub ctx: &'a Context<'a, 'ctx>,
    pub emit: &'a ProgEmit<'a, 'ctx>,
    pub var_allocs: IndexVec<VarId, inkwell::values::PointerValue<'ctx>>,
    pub locals: IndexVec<LocalId, Option<inkwell::values::BasicValueEnum<'ctx>>>,
    pub blocks: IndexVec<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    pub func: inkwell::values::FunctionValue<'ctx>,
    pub proc: &'a Proc,
}

impl<'a, 'ctx> ProcEmit<'a, 'ctx> {
    pub fn new(ctx: &'a Context<'a, 'ctx>, emit: &'a ProgEmit<'a, 'ctx>, proc: &'a Proc, func: inkwell::values::FunctionValue<'ctx>) -> Self {
        Self {
            ctx: ctx,
            emit: emit,
            var_allocs: IndexVec::new(),
            locals: IndexVec::new(),
            blocks: IndexVec::new(),
            func: func,
            proc: proc,
        }
    }

    fn emit_entry_block(&mut self) -> inkwell::basic_block::BasicBlock<'a> {
        let block = self.ctx.llvm_ctx.append_basic_block(self.func, "entry");
        self.ctx.builder.position_at_end(block);

        let null_val = self.ctx.rt.ty.val_type.const_zero();

        for local in self.proc.locals.iter() {
            self.locals.push(None);
        }

        for var in self.proc.vars.iter() {
            let name = &if var.name.id() != 0 {
                format!("var_{}", self.emit.env.string_table.get(var.name))
            } else {
                format!("ret_var")
            };

            let alloca = match var.ty {
                ty::Complex::Primitive(prim) => {
                    match prim {
                        ty::Primitive::Float => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.f32_type(), name),
                        ty::Primitive::String => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.i64_type(), name),
                        ty::Primitive::Ref(_) => self.ctx.builder.build_alloca(self.ctx.rt.ty.datum_common_type_ptr, name),
                        _ => unimplemented!("unhandled prim: {:?}", prim),
                    }
                },
                ty::Complex::Any | ty::Complex::OneOf(_) => {
                    let alloc = self.ctx.builder.build_alloca(self.ctx.rt.ty.val_type, name);
                    self.ctx.builder.build_store(alloc, null_val);
                    alloc
                },
                _ => unimplemented!("unhandled ty: {:?}", var.ty),
            };
            self.var_allocs.push(alloca);
        }

        let param_locals_arr_ty = self.ctx.rt.ty.val_type.ptr_type(inkwell::AddressSpace::Generic).array_type(self.proc.params.len() as u32);
        let mut param_locals_arr = param_locals_arr_ty.const_zero();

        for (i, var_id) in self.proc.params.iter().enumerate() {
            let alloc = self.var_allocs[*var_id];
            // TODO: handle keyword args
            param_locals_arr = self.ctx.builder.build_insert_value(param_locals_arr, alloc, i as u32, "param_locals_arr").unwrap().into_array_value();
        }

        let param_locals_alloca = self.ctx.builder.build_alloca(param_locals_arr_ty, "param_locals_alloca");
        self.ctx.builder.build_store(param_locals_alloca, param_locals_arr);

        let cz = self.ctx.llvm_ctx.i32_type().const_zero();
        let param_locals_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(param_locals_alloca, &[cz, cz], "param_locals_ptr") };
        let argpack_local = self.func.get_params()[0];

        self.ctx.builder.build_call(self.ctx.rt.rt_arg_pack_unpack_into, &[argpack_local, param_locals_ptr.into(), self.ctx.llvm_ctx.i64_type().const_int(self.proc.id.index() as u64, false).into(), self.emit.rt_global.as_pointer_value().into()], "");

        block
    }

    fn finalize_entry_block(&self, entry: inkwell::basic_block::BasicBlock) {
        self.ctx.builder.position_at_end(entry);
        self.ctx.builder.build_unconditional_branch(self.blocks[BlockId::new(0)]);
    }

    pub fn emit_proc(&mut self, emit: &ProgEmit<'_, 'ctx>) {
        let entry_bb = self.emit_entry_block();

        for cfg_block in self.proc.blocks.iter() {
            let block = self.ctx.llvm_ctx.append_basic_block(self.func, &format!("s{}b{}", cfg_block.scope.index(), cfg_block.id.index()));
            self.blocks.push(block);
        }

        for (id, block) in self.proc.blocks.iter_enumerated() {
            let ll_block = self.blocks[id];
            self.ctx.builder.position_at_end(ll_block);
            self.emit_block(block, emit);
        }

        self.finalize_entry_block(entry_bb);
    }

    fn assign_literal(&mut self, lit: &Literal, local_id: LocalId) {
        let val = match lit {
            Literal::Num(x) => {
                let lit_val = self.ctx.llvm_ctx.f32_type().const_float(*x as f64).into();
                Value::new(Some(lit_val), ty::Primitive::Float.into())
            },
            Literal::String(id) => {
                let lit_val = self.ctx.llvm_ctx.i64_type().const_int(id.id(), false).into();
                Value::new(Some(lit_val), ty::Primitive::String.into())
            },
            Literal::Null => {
                Value::new(Some(self.ctx.llvm_ctx.i64_type().const_zero().into()), ty::Primitive::Null.into())
            },
            _ => unimplemented!("{:?}", lit),
        };

        self.set_local(local_id, &val);
    }

    // TODO: refactor out as_any, bad.
    fn get_local(&self, local: LocalId, as_any: bool) -> Value<'ctx> {
        let ty = match as_any {
            true => ty::Complex::Any,
            false => self.proc.locals[local].ty.clone(),
        };
        let val = match as_any {
            true => self.conv_val(&self.get_local(local, false), &ty::Complex::Any),
            false => self.locals[local].unwrap(),
        };

        Value::new(Some(val), ty)
    }

    /// Shortcut method to get an Any llval for a local
    fn get_local_any(&self, local: LocalId) -> inkwell::values::BasicValueEnum<'ctx> {
        self.get_local(local, true).val.unwrap()
    }

    fn set_local(&mut self, local_id: LocalId, val: &Value<'ctx>) {
        let local_ty = &self.proc.locals[local_id].ty;
        assert!(self.locals[local_id].is_none());
        self.locals[local_id] = Some(self.conv_val(val, local_ty));
    }

    fn load_var(&self, var_id: VarId) -> Value<'ctx> {
        let var_ptr = self.var_allocs[var_id];
        let var_ty = &self.proc.vars[var_id].ty;
        let llval = self.ctx.builder.build_load(var_ptr, "");
        Value::new(Some(llval), var_ty.clone())
    }

    fn store_var(&self, var_id: VarId, val: &Value<'ctx>) {
        let var_ptr = self.var_allocs[var_id];
        let var_ty = &self.proc.vars[var_id].ty;
        let llval = self.conv_val(val, var_ty);
        self.ctx.builder.build_store(var_ptr, llval);
    }

    fn conv_val(&self, val: &Value<'ctx>, ty: &ty::Complex) -> inkwell::values::BasicValueEnum<'ctx> {
        // TODO revisit this as types expand, some assumptions are shaky
        // the panics here indicate an error in type unification, not a user error
        if *ty == val.ty || (ty.is_any() && val.ty.is_any()) {
            val.val.unwrap()
            //if let Some(ll_val) = val.val {
        } else if val.ty.is_any() {
            panic!("attempted to store Any val with ty {:?} in non-Any local with ty {:?}", val.ty, ty)
        } else if let Some(val_prim) = val.ty.as_primitive() {
            if let Some(local_prim) = ty.as_primitive() {
                // autocast?
                // useless check, just for code clarity
                if val_prim != local_prim {
                    panic!("primitive val (ty {:?}) does not match primitive local (ty {:?})", val.ty, ty);
                }
            }
            if ty.contains(val_prim) {
                let val_val = val.val.unwrap();

                let (disc, llval) = match val_prim {
                    ty::Primitive::Null => {
                        let null_val = self.ctx.llvm_ctx.i64_type().const_zero();
                        // TODO: less magic for disc vals
                        (0, null_val)
                    },
                    ty::Primitive::Float => {
                        // TODO fix this, this is mega bad, won't work on big endian systems
                        let val_as_i32 = self.ctx.builder.build_bitcast(val_val, self.ctx.llvm_ctx.i32_type(), "pack_val");
                        let val_as_i64: inkwell::values::IntValue = self.ctx.builder.build_int_z_extend(val_as_i32.into_int_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");
                        (1, val_as_i64)
                    },
                    ty::Primitive::String => {
                        let val_as_int = self.ctx.builder.build_bitcast(val_val, self.ctx.llvm_ctx.i64_type(), "pack_val").into_int_value();
                        (2, val_as_int)
                    },
                    ty::Primitive::Ref(_) => {
                        let val_as_int: inkwell::values::IntValue = self.ctx.builder.build_ptr_to_int(val_val.into_pointer_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");
                        (3, val_as_int)
                    },
                    _ => unimplemented!(),
                };

                let mut local_struct = self.ctx.rt.ty.val_type.const_zero();
                local_struct = self.ctx.builder.build_insert_value(local_struct, self.ctx.llvm_ctx.i32_type().const_int(disc, false), 0, "").unwrap().into_struct_value();
                local_struct = self.ctx.builder.build_insert_value(local_struct, llval, 1, "").unwrap().into_struct_value();

                local_struct.into()
            } else {
                panic!("primitive val (ty {:?}) does not fit in local (ty {:?})", val.ty, ty);
            }
        } else {
            unimplemented!("unimplemented store case: {:?} -> {:?}", val.ty, ty);
        }
    }

    fn emit_block(&mut self, block: &Block, emit: &ProgEmit<'_, 'ctx>) {
        for op in block.ops.iter() {
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
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[copy_any], "");
                    }
                },

                Op::Store(var, local) => {
                    let var_ty = &self.proc.vars[*var].ty;
                    if var_ty.needs_destructor() {
                        let val = self.load_var(*var);
                        let val_any = self.conv_val(&val, &ty::Complex::Any);
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[val_any], "");
                    }

                    let store_val = self.get_local(*local, false);
                    if store_val.ty.needs_destructor() {
                        let store_val_any = self.conv_val(&store_val, &ty::Complex::Any);
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[store_val_any], "");
                    }
                    self.store_var(*var, &store_val);
                },

                Op::Put(id) => {
                    let norm = self.get_local_any(*id);

                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &[norm.into(), self.emit.rt_global.as_pointer_value().into()], "put");
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.get_local_any(*lhs).into();
                    let rhs_ref = self.get_local_any(*rhs).into();
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false).into();

                    let res = self.ctx.builder.build_call(self.ctx.rt.rt_val_binary_op, &[self.emit.rt_global.as_pointer_value().into(), op_var, lhs_ref, rhs_ref], "").try_as_basic_value().left().unwrap();
                    self.set_local(*id, &Value::new(Some(res), ty::Complex::Any));
                },

                Op::HardBinary(id, op, lhs, rhs) => {
                    let lhs_val = self.get_local(*lhs, false);
                    let rhs_val = self.get_local(*rhs, false);
                    let res = self.emit_binary(*op, &lhs_val, &rhs_val);
                    self.set_local(*id, &res);
                },

                Op::Call(id, name, args) => {
                    let argpack_ptr = self.build_argpack(&args);

                    // TODO: better proc lookup, consider src
                    let func = emit.lookup_global_proc(*name);
                    let res_val = self.ctx.builder.build_call(func, &[argpack_ptr.into()], "res").try_as_basic_value().left().unwrap();
                    let val = Value::new(Some(res_val), ty::Complex::Any);
                    self.set_local(*id, &val);
                },

                Op::Cast(dst, src, ty) => {
                    // TODO: impl eq for prim<->complex
                    assert!(self.proc.locals[*dst].ty == (*ty).into());
                    if *ty != ty::Primitive::String {
                        unimplemented!("can only cast to string, not {:?}", ty);
                    }

                    let src_llval = self.get_local_any(*src);
                    let res_llval = self.ctx.builder.build_call(self.ctx.rt.rt_val_cast_string_val, &[src_llval, self.emit.rt_global.as_pointer_value().into()], "").try_as_basic_value().left().unwrap();
                    let res_val = Value::new(Some(res_llval), (*ty).into());
                    self.set_local(*dst, &res_val);
                },

                Op::AllocDatum(dst, ty_id) => {
                    // TODO: impl eq for prim<->complex
                    // TODO: assert actual type once inference pops it
                    assert!(self.proc.locals[*dst].ty == ty::Primitive::Ref(None).into());
                    let datum_ptr = self.ctx.builder.build_call(self.ctx.rt.rt_runtime_alloc_datum, &[
                        self.emit.rt_global.as_pointer_value().into(),
                        self.ctx.llvm_ctx.i32_type().const_int(ty_id.index() as u64, false).into(),
                    ], "datum_ptr").try_as_basic_value().left().unwrap();
                    let ref_val = Value::new(Some(datum_ptr), ty::Primitive::Ref(None).into());
                    self.set_local(*dst, &ref_val);
                },

                Op::DatumLoadVar(dst, src, var_id) => {
                    let val = self.get_local_any(*src).into_struct_value();
                    let ref_ptr = self.build_extract_ref_ptr(val);
                    let var_get_ptr = self.build_vtable_lookup(ref_ptr, vtable::VTABLE_VAR_GET_FIELD_OFFSET).into_pointer_value();

                    // TODO: drop/cloned

                    let var_val = self.ctx.builder.build_call(var_get_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.id(), false).into(),
                    ], "var_val").try_as_basic_value().left().unwrap().into_struct_value();
                    self.set_local(*dst, &Value::new(Some(var_val.into()), ty::Complex::Any));
                },

                Op::DatumStoreVar(dst, var_id, src) => {
                    let dst_val = self.get_local_any(*dst).into_struct_value();
                    let ref_ptr = self.build_extract_ref_ptr(dst_val);
                    let var_set_ptr = self.build_vtable_lookup(ref_ptr, vtable::VTABLE_VAR_SET_FIELD_OFFSET).into_pointer_value();

                    let src_val = self.get_local_any(*src);
                    self.ctx.builder.build_call(var_set_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.id(), false).into(),
                        src_val,
                    ], "");
                },

                Op::DatumCallProc(dst, src, proc_name, args) => {
                    let argpack_ptr = self.build_argpack(&args);

                    let src_val = self.get_local_any(*src).into_struct_value();
                    let ref_ptr = self.build_extract_ref_ptr(src_val);
                    let proc_lookup_ptr = self.build_vtable_lookup(ref_ptr, vtable::VTABLE_PROC_LOOKUP_FIELD_OFFSET).into_pointer_value();

                    let proc_ptr = self.ctx.builder.build_call(proc_lookup_ptr, &[
                        self.ctx.llvm_ctx.i32_type().const_int(proc_name.id(), false).into(),
                    ], "").try_as_basic_value().left().unwrap().into_pointer_value();

                    let res_val = self.ctx.builder.build_call(proc_ptr, &[
                        argpack_ptr.into(),
                    ], "res").try_as_basic_value().left().unwrap();

                    self.set_local(*dst, &Value::new(Some(res_val.into()), ty::Complex::Any));
                },

                //_ => unimplemented!("{:?}", op),
            }
        }

        match &block.terminator {
            Terminator::Return => {
                self.finalize_block(block);
                let ret = self.load_var(VarId::new(0));
                let ret_any = self.conv_val(&ret, &ty::Complex::Any);
                self.ctx.builder.build_return(Some(&ret_any));
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
                            self.ctx.builder.build_cast(inkwell::values::InstructionOpcode::FPToSI, disc_val.val.unwrap(), self.ctx.llvm_ctx.i32_type(), "disc").into_int_value()
                        },
                        _ => unimplemented!("{:?}", prim_ty),
                    }
                } else {
                    let disc_val = self.get_local_any(*discriminant);
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_to_switch_disc, &[disc_val], "disc").try_as_basic_value().left().unwrap().into_int_value()
                };

                self.finalize_block(block);

                // convert branches to use llvm blocks
                let llvm_branches = branches.iter().map(|(lit, target)| {
                    let val = self.ctx.llvm_ctx.i32_type().const_int((*lit).into(), false);
                    (val, self.blocks[*target])
                }).collect::<Vec<_>>();

                self.ctx.builder.build_switch(disc_int, self.blocks[*default], &llvm_branches[..]);
            },
        }
    }

    fn emit_binary(&self, op: ty::op::HardBinary, lhs: &Value<'ctx>, rhs: &Value<'ctx>) -> Value<'ctx> {
        match op {
            ty::op::HardBinary::StringConcat => {
                let res = self.ctx.builder.build_call(self.ctx.rt.rt_runtime_concat_strings, &[self.emit.rt_global.as_pointer_value().into(), lhs.val.unwrap(), rhs.val.unwrap()], "").try_as_basic_value().left().unwrap();
                Value::new(Some(res), ty::Primitive::String.into())
            }
            ty::op::HardBinary::FloatAdd => {
                let res = self.ctx.builder.build_float_add(lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "").into();
                Value::new(Some(res), ty::Primitive::Float.into())
            }
            ty::op::HardBinary::FloatSub => {
                let res = self.ctx.builder.build_float_sub(lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "").into();
                Value::new(Some(res), ty::Primitive::Float.into())
            }
            ty::op::HardBinary::FloatMul => {
                let res = self.ctx.builder.build_float_mul(lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "").into();
                Value::new(Some(res), ty::Primitive::Float.into())
            }
            ty::op::HardBinary::FloatDiv => {
                let res = self.ctx.builder.build_float_div(lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "").into();
                Value::new(Some(res), ty::Primitive::Float.into())
            }
            ty::op::HardBinary::FloatMod => {
                let res = self.ctx.builder.build_float_rem(lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "").into();
                Value::new(Some(res), ty::Primitive::Float.into())
            }
            ty::op::HardBinary::FloatPow => {
                unimplemented!();
            }
            ty::op::HardBinary::FloatCmp(pred) => {
                let bool_res = self.ctx.builder.build_float_compare(pred, lhs.val.unwrap().into_float_value(), rhs.val.unwrap().into_float_value(), "");
                let float_res = self.ctx.builder.build_unsigned_int_to_float(bool_res, self.ctx.llvm_ctx.f32_type(), "");
                Value::new(Some(float_res.into()), ty::Primitive::Float.into())
            }
        }
    }

    fn build_argpack(&self, args: &[LocalId]) -> inkwell::values::PointerValue<'ctx> {
        let n_val = self.ctx.llvm_ctx.i64_type().const_int(args.len() as u64, false);
        let array_type = self.ctx.rt.ty.val_type.array_type(args.len() as u32);
        let mut argpack_unnamed = array_type.const_zero();
        for (i, arg) in args.iter().enumerate() {
            argpack_unnamed = self.ctx.builder.build_insert_value(argpack_unnamed, self.get_local_any(*arg), i as u32, "arg").unwrap().into_array_value();
        }
        let argpack_unnamed_alloca = self.ctx.builder.build_alloca(array_type, "argpack_unnamed_arr");
        self.ctx.builder.build_store(argpack_unnamed_alloca, argpack_unnamed);

        // this GEPpery is bad?
        let cz = self.ctx.llvm_ctx.i32_type().const_zero();
        let argpack_unnamed_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(argpack_unnamed_alloca, &[cz, cz], "argpack_unnamed_ptr") };

        let argpack_alloca = self.ctx.builder.build_alloca(self.ctx.rt.ty.arg_pack_type, "argpack_ptr");
        let argpack = self.ctx.rt.ty.arg_pack_type.const_zero();
        let argpack = self.ctx.builder.build_insert_value(argpack, n_val, 0, "argpack").unwrap();
        let argpack = self.ctx.builder.build_insert_value(argpack, argpack_unnamed_ptr, 1, "argpack").unwrap();

        self.ctx.builder.build_store(argpack_alloca, argpack);

        argpack_alloca
    }

    fn build_extract_ref_ptr(&self, val: inkwell::values::StructValue<'ctx>) -> inkwell::values::PointerValue<'ctx> {
        // TODO: tycheck we're a ref
        // TODO: use some constant instead of 1
        // TODO: TYAPI
        // TODO: cast val struct instead of int2ptr
        let ref_ptr_int = self.ctx.builder.build_extract_value(val, 1, "ref_ptr_int").unwrap().into_int_value();
        let ref_ptr = self.ctx.builder.build_int_to_ptr(ref_ptr_int, self.ctx.rt.ty.datum_common_type_ptr, "ref_ptr");
        ref_ptr
    }

    fn build_extract_ty_id(&self, datum_ptr: inkwell::values::PointerValue<'ctx>) -> inkwell::values::IntValue<'ctx> {
        let ty_id_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(datum_ptr, &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            self.ctx.llvm_ctx.i32_type().const_int(datum::DATUM_TY_FIELD_OFFSET, false),
        ], "ty_id_ptr") };
        let ty_id = self.ctx.builder.build_load(ty_id_ptr, "ty_id").into_int_value();
        ty_id
    }

    fn build_vtable_lookup_inline(&self, ty_id: inkwell::values::IntValue<'ctx>, offset: u64) -> inkwell::values::BasicValueEnum<'ctx> {
        let field_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(self.emit.vt_global.as_pointer_value(), &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            ty_id,
            self.ctx.llvm_ctx.i32_type().const_int(offset, false),
        ], &format!("vtable_field_{}_ptr", offset)) };
        let field = self.ctx.builder.build_load(field_ptr, &format!("vtable_field_{}", offset));
        field
    }

    fn build_vtable_lookup(&self, datum_ptr: inkwell::values::PointerValue<'ctx>, offset: u64) -> inkwell::values::BasicValueEnum<'ctx> {
        let lookup_fn = self.emit.vt_lookup[offset as usize];
        let field = self.ctx.builder.build_call(lookup_fn, &[datum_ptr.into()], &format!("vtable_field_{}", offset)).try_as_basic_value().left().unwrap();
        field
    }

    fn finalize_block(&self, block: &Block) {
        if block.scope_end {
            for local_id in self.proc.scopes[block.scope].destruct_locals.iter() {
                if self.locals[*local_id].is_none() {
                    //panic!("local {:?} is never set", local_id);
                }
                let local = &self.proc.locals[*local_id];
                if local.ty.needs_destructor() {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[self.get_local_any(*local_id)], "");
                }
            }
            for var_id in self.proc.scopes[block.scope].destruct_vars.iter() {
                let var = &self.proc.vars[*var_id];
                if var.ty.needs_destructor() {
                    let var_val = self.load_var(*var_id);
                    let var_any = self.conv_val(&var_val, &ty::Complex::Any);
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[var_any], "");
                }
            }
        }
    }
}
