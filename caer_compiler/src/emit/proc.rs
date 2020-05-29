use std::borrow::Borrow;
use super::context::Context;
use super::value::Value;
use super::prog::ProgEmit;
use indexed_vec::{IndexVec, Idx};
use crate::ir::cfg::*;
use crate::ir::id::*;
use crate::ty::{self, Ty};
use caer_runtime::string_table::StringId;
use caer_runtime::datum;
use caer_runtime::vtable;

#[derive(Debug)]
pub struct ProcEmit<'a, 'ctx> {
    pub ctx: &'a Context<'a, 'ctx>,
    pub emit: &'a ProgEmit<'a, 'ctx>,
    pub local_allocs: IndexVec<LocalId, inkwell::values::PointerValue<'ctx>>,
    pub blocks: IndexVec<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    pub func: inkwell::values::FunctionValue<'ctx>,
    pub proc: &'a Proc,
}

impl<'a, 'ctx> ProcEmit<'a, 'ctx> {
    pub fn new(ctx: &'a Context<'a, 'ctx>, emit: &'a ProgEmit<'a, 'ctx>, proc: &'a Proc, func: inkwell::values::FunctionValue<'ctx>) -> Self {
        Self {
            ctx: ctx,
            emit: emit,
            local_allocs: IndexVec::new(),
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
            let name = &match &local.var {
                Some(var_info) => {
                    if var_info.name.id() != 0 {
                        format!("var_{}", self.emit.env.string_table.get(var_info.name))
                    } else {
                        format!("ret_var")
                    }
                },
                None => format!("local_{}", local.id.index()),
            };

            let alloc = match local.ty {
                ty::Complex::Primitive(prim) => {
                    match prim {
                        ty::Primitive::Float => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.f32_type(), name),
                        ty::Primitive::String => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.i64_type(), name),
                        ty::Primitive::Ref(_) => self.ctx.builder.build_alloca(self.ctx.rt.ty.datum_common_type_ptr, name),
                        _ => unimplemented!("unhandled prim: {:?}", prim),
                    }
                },
                ty::Complex::Any => {
                    let alloc = self.ctx.builder.build_alloca(self.ctx.rt.ty.val_type, name);
                    self.ctx.builder.build_store(alloc, null_val);
                    alloc
                },
                _ => unimplemented!("unhandled ty: {:?}", local.ty),
            };

            self.local_allocs.push(alloc);
        }

        let param_locals_arr_ty = self.ctx.rt.ty.val_type.ptr_type(inkwell::AddressSpace::Generic).array_type(self.proc.params.len() as u32);
        let mut param_locals_arr = param_locals_arr_ty.const_zero();

        for (i, local_id) in self.proc.params.iter().enumerate() {
            let alloc = self.local_allocs[*local_id];
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

    pub fn emit_proc(&mut self, emit: &ProgEmit) {
        let entry_bb = self.emit_entry_block();

        for cfg_block in self.proc.blocks.iter() {
            let block = self.ctx.llvm_ctx.append_basic_block(self.func, &format!("s{}b{}", cfg_block.scope.index(), cfg_block.id.index()));
            self.blocks.push(block);
        }

        for (id, block) in self.proc.blocks.iter().enumerate() { // TODO port iter_enumerated
            let ll_block = self.blocks[BlockId::new(id)];
            self.ctx.builder.position_at_end(ll_block);
            self.emit_block(block, emit);
        }

        self.finalize_entry_block(entry_bb);
    }

    fn assign_literal(&self, lit: &Literal, local_id: LocalId) {
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
                Value::new(None, ty::Primitive::Null.into())
            },
            _ => unimplemented!("{:?}", lit),
        };

        self.store_local(local_id, &val);
    }

    // TODO: refactor out as_any, bad.
    fn load_local<L: Borrow<LocalId>>(&self, local: L, as_any: bool) -> Value<'ctx> {
        let ty = match as_any {
            true => ty::Complex::Any,
            false => self.proc.locals[*local.borrow()].ty.clone(),
        };
        let alloc = match as_any {
            true => self.convert_to_any(*local.borrow()),
            false => self.local_allocs[*local.borrow()],
        };
        let val = self.ctx.builder.build_load(alloc, "").into();

        Value::new(val, ty)
    }

    fn store_local<L: Borrow<LocalId>>(&self, local_id: L, val: &Value) {
        let local_id = *local_id.borrow();
        let local_ty = &self.proc.locals[local_id].ty;
        let local_ptr = self.local_allocs[local_id];
        self.store_val(val, local_ty, local_ptr);
    }

    fn convert_to_any(&self, local_id: LocalId) -> inkwell::values::PointerValue<'ctx> {
        let local = &self.proc.locals[local_id];

        if local.ty == ty::Complex::Any {
            return self.local_allocs[local_id];
        }

        let val = self.load_local(local_id, false);
        let temp_alloca = self.ctx.builder.build_alloca(self.ctx.rt.ty.val_type, "temp");
        self.store_val(&val, &ty::Complex::Any, temp_alloca);
        temp_alloca
    }

    fn store_val(&self, val: &Value, ty: &ty::Complex, ptr: inkwell::values::PointerValue) {
        // TODO revisit this as types expand, some assumptions are shaky
        // the panics here indicate an error in type unification, not a user error
        if *ty == val.ty {
            if let Some(ll_val) = val.val {
                self.ctx.builder.build_store(ptr, ll_val);
            }
        } else if val.ty == ty::Complex::Any {
            panic!("attempted to store Any val in non-Any local")
        } else if let Some(val_prim) = val.ty.as_primitive() {
            if let Some(local_prim) = ty.as_primitive() {
                // autocast?
                // useless check, just for code clarity
                if val_prim != local_prim {
                    panic!("primitive val (ty {:?}) does not match primitive local (ty {:?})", val.ty, ty);
                }
            }
            if ty.contains(val_prim) {
                let local_val = self.ctx.builder.build_load(ptr, "val");
                let local_struct = local_val.into_struct_value();

                let local_upd = match val_prim {
                    ty::Primitive::Null => {
                        // TODO: less magic for disc vals
                        let null_disc = self.ctx.llvm_ctx.i32_type().const_int(0, false);
                        let upd = self.ctx.builder.build_insert_value(local_struct, null_disc, 0, "upd_disc").unwrap();
                        upd
                    },
                    ty::Primitive::Float => {
                        let float_disc = self.ctx.llvm_ctx.i32_type().const_int(1, false);
                        // TODO fix this, this is mega bad, won't work on big endian systems
                        let val_as_i32 = self.ctx.builder.build_bitcast(val.val.unwrap(), self.ctx.llvm_ctx.i32_type(), "pack_val");
                        let val_as_i64: inkwell::values::IntValue = self.ctx.builder.build_int_z_extend(val_as_i32.into_int_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");
                        let upd = self.ctx.builder.build_insert_value(local_struct, float_disc, 0, "upd_disc").unwrap();
                        // TODO: revisit this 2. it's here because of padding in the val struct -
                        // use offset_of?
                        let upd = self.ctx.builder.build_insert_value(upd, val_as_i64, 2, "upd_val").unwrap();
                        upd
                    },
                    ty::Primitive::String => {
                        let string_disc = self.ctx.llvm_ctx.i32_type().const_int(2, false);
                        let val_as_int = self.ctx.builder.build_bitcast(val.val.unwrap(), self.ctx.llvm_ctx.i64_type(), "pack_val");

                        let upd = self.ctx.builder.build_insert_value(local_struct, string_disc, 0, "upd_disc").unwrap();
                        let upd = self.ctx.builder.build_insert_value(upd, val_as_int, 2, "upd_val").unwrap();

                        upd
                    },
                    ty::Primitive::Ref(_) => {
                        let ref_disc = self.ctx.llvm_ctx.i32_type().const_int(3, false);
                        let val_as_int: inkwell::values::IntValue = self.ctx.builder.build_ptr_to_int(val.val.unwrap().into_pointer_value(), self.ctx.llvm_ctx.i64_type(), "pack_val");

                        let upd = self.ctx.builder.build_insert_value(local_struct, ref_disc, 0, "upd_disc").unwrap();
                        let upd = self.ctx.builder.build_insert_value(upd, val_as_int, 2, "upd_val").unwrap();

                        upd
                    },
                    _ => unimplemented!(),
                };

                self.ctx.builder.build_store(ptr, local_upd);
            } else {
                panic!("primitive val (ty {:?}) does not fit in local (ty {:?})", val.ty, ty);
            }
        } else {
            unimplemented!("unimplemented store case: {:?} -> {:?}", val.ty, ty);
        }
    }

    fn emit_block(&self, block: &Block, emit: &ProgEmit) {
        for op in block.ops.iter() {
            match op {
                Op::Literal(id, literal) => {
                    self.assign_literal(literal, *id);
                },

                Op::MkVar(_) => {
                    // nothing now - can we remove this?
                }

                Op::Load(local, var) => {
                    let copy = self.load_local(var, false);
                    self.store_local(local, &copy);
                    if copy.ty.needs_destructor() {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*local].into()], "");
                    }
                },

                Op::Store(var, local) => {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[self.local_allocs[*var].into()], "");

                    let copy = self.load_local(local, false);
                    self.store_local(var, &copy);

                    if copy.ty.needs_destructor() && self.proc.locals[*local].movable {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*var].into()], "");
                    }
                },

                Op::Put(id) => {
                    let norm = self.convert_to_any(*id);

                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &[norm.into(), self.emit.rt_global.as_pointer_value().into()], "put");
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.convert_to_any(*lhs).into();
                    let rhs_ref = self.convert_to_any(*rhs).into();
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false).into();

                    self.ctx.builder.build_call(self.ctx.rt.rt_val_binary_op, &[self.local_allocs[*id].into(), self.emit.rt_global.as_pointer_value().into(), op_var, lhs_ref, rhs_ref], "");
                },

                Op::Call(id, name, args) => {
                    let argpack_ptr = self.build_argpack(&args);

                    // TODO: don't lookup procs by symbol table like this
                    let func = emit.lookup_global_proc(*name);
                    let res_val = self.ctx.builder.build_call(func, &[argpack_ptr.into()], "res").try_as_basic_value().left().unwrap();
                    let val = Value::new(Some(res_val), ty::Complex::Any);
                    self.store_local(id, &val);
                },

                Op::Cast(dst, src, ty) => {
                    if *ty != ty::Primitive::String {
                        unimplemented!("can only cast to string, not {:?}", ty);
                    }

                    let src_ref = self.convert_to_any(*src).into();
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_cast_string_val, &[self.local_allocs[*dst].into(), src_ref, self.emit.rt_global.as_pointer_value().into()], "");
                },

                Op::AllocDatum(dst, ty_id) => {
                    let datum_ptr = self.ctx.builder.build_call(self.ctx.rt.rt_runtime_alloc_datum, &[
                        self.emit.rt_global.as_pointer_value().into(),
                        self.ctx.llvm_ctx.i32_type().const_int(ty_id.index() as u64, false).into(),
                    ], "datum_ptr").try_as_basic_value().left().unwrap();
                    let ref_val = Value::new(Some(datum_ptr), ty::Primitive::Ref(Some(*ty_id)).into());
                    self.store_local(dst, &ref_val);
                },

                Op::DatumLoadVar(dst, src, var_id) => {
                    let val = self.load_local(src, true);
                    let ref_ptr = self.build_extract_ref_ptr(val.val.unwrap().into_struct_value());
                    let ty_id = self.build_extract_ty_id(ref_ptr);
                    let var_get_ptr = self.build_vtable_lookup(ty_id, vtable::VTABLE_VAR_GET_FIELD_OFFSET).into_pointer_value();

                    let var_val = self.ctx.builder.build_call(var_get_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.id(), false).into(),
                    ], "var_val").try_as_basic_value().left().unwrap().into_struct_value();
                    self.store_local(dst, &Value::new(Some(var_val.into()), ty::Complex::Any));
                },

                Op::DatumStoreVar(dst, var_id, src) => {
                    let dst_val = self.load_local(dst, true);
                    let ref_ptr = self.build_extract_ref_ptr(dst_val.val.unwrap().into_struct_value());
                    let ty_id = self.build_extract_ty_id(ref_ptr);
                    let var_set_ptr = self.build_vtable_lookup(ty_id, vtable::VTABLE_VAR_SET_FIELD_OFFSET).into_pointer_value();

                    let src_val = self.load_local(src, true);
                    self.ctx.builder.build_call(var_set_ptr, &[
                        ref_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().const_int(var_id.id(), false).into(),
                        src_val.val.unwrap(),
                    ], "");
                },

                Op::DatumCallProc(dst, src, proc_name, args) => {
                    let argpack_ptr = self.build_argpack(&args);

                    let src_val = self.load_local(src, true);
                    let ref_ptr = self.build_extract_ref_ptr(src_val.val.unwrap().into_struct_value());
                    let ty_id = self.build_extract_ty_id(ref_ptr);
                    let proc_lookup_ptr = self.build_vtable_lookup(ty_id, vtable::VTABLE_PROC_LOOKUP_FIELD_OFFSET).into_pointer_value();

                    let proc_ptr = self.ctx.builder.build_call(proc_lookup_ptr, &[
                        self.ctx.llvm_ctx.i64_type().const_int(proc_name.id(), false).into(),
                    ], "").try_as_basic_value().left().unwrap().into_pointer_value();

                    let res_val = self.ctx.builder.build_call(proc_ptr, &[
                        argpack_ptr.into(),
                    ], "res").try_as_basic_value().left().unwrap();

                    self.store_local(dst, &Value::new(Some(res_val.into()), ty::Complex::Any));
                },

                //_ => unimplemented!("{:?}", op),
            }
        }

        match &block.terminator {
            Terminator::Return => {
                self.finalize_block(block);
                let ret = self.ctx.builder.build_load(self.local_allocs[LocalId::new(0)], "ret");
                self.ctx.builder.build_return(Some(&ret));
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
                            let disc_val = self.ctx.builder.build_load(self.local_allocs[*discriminant], "disc_float");
                            self.ctx.builder.build_cast(inkwell::values::InstructionOpcode::FPToSI, disc_val, self.ctx.llvm_ctx.i32_type(), "disc").into_int_value()
                        },
                        _ => unimplemented!("{:?}", prim_ty),
                    }
                } else {
                    let disc_val = self.ctx.builder.build_call(self.ctx.rt.rt_val_to_switch_disc, &[self.local_allocs[*discriminant].into()], "disc").try_as_basic_value().left().unwrap();
                    disc_val.into_int_value()
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

    fn build_argpack(&self, args: &[LocalId]) -> inkwell::values::PointerValue<'ctx> {
        let n_val = self.ctx.llvm_ctx.i64_type().const_int(args.len() as u64, false);
        let array_type = self.ctx.rt.ty.val_type.ptr_type(inkwell::AddressSpace::Generic).array_type(args.len() as u32);
        let argpack_unnamed_alloca = self.ctx.builder.build_alloca(array_type, "argpack_unnamed_arr");
        let mut argpack_unnamed = array_type.const_zero();
        for (i, arg) in args.iter().enumerate() {
            argpack_unnamed = self.ctx.builder.build_insert_value(argpack_unnamed, self.convert_to_any(*arg), i as u32, "arg").unwrap().into_array_value();
        }
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
        // TODO: use some constant instead of 2
        // TODO: TYAPI
        // TODO: cast val struct instead of int2ptr
        let ref_ptr_int = self.ctx.builder.build_extract_value(val, 2, "ref_ptr_int").unwrap().into_int_value();
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

    fn build_vtable_lookup(&self, ty_id: inkwell::values::IntValue<'ctx>, offset: u64) -> inkwell::values::BasicValueEnum<'ctx> {
        let field_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(self.emit.vt_global.as_pointer_value(), &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            ty_id,
            self.ctx.llvm_ctx.i32_type().const_int(offset, false),
        ], &format!("vtable_field_{}_ptr", offset)) };
        let field = self.ctx.builder.build_load(field_ptr, &format!("vtable_field_{}", offset));
        field
    }

    fn finalize_block(&self, block: &Block) {
        if block.scope_end {
            for local_id in self.proc.scopes[block.scope].destruct_locals.iter() {
                let local = &self.proc.locals[*local_id];
                if local.ty.needs_destructor() {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[self.local_allocs[*local_id].into()], "");
                }
            }
        }
    }
}
