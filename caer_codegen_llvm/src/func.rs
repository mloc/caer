use caer_ir::cfg::*;
use caer_ir::id::{BlockId, LocalId, VarId};
use caer_ir::walker::{CFGWalker, Lifetimes, WalkActor};
use caer_types::id::{FuncId, StringId, TypeId};
use caer_types::ty::{self, Layout, RefType, ScalarLayout};
use caer_types::{layout, op};
use index_vec::IndexVec;
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicType, PointerType};
use inkwell::values::*;
use ty::Type;

use crate::context::{Context, GC_ADDRESS_SPACE};
use crate::prog::{Intrinsic, ProgEmit};
use crate::value::{BveWrapper, StackValue};

#[derive(Debug)]
pub struct FuncEmit<'a, 'p, 'ctx> {
    ctx: &'a Context<'a, 'ctx>,
    ir_func: &'a Function,
    prog_emit: &'a ProgEmit<'p, 'ctx>,

    var_allocs: IndexVec<VarId, StackValue<'ctx>>,
    locals: IndexVec<LocalId, Option<BasicValueEnum<'ctx>>>,
    jumper: Option<InstructionValue<'ctx>>,
    blocks: IndexVec<BlockId, BasicBlock<'ctx>>,
    ll_func: FunctionValue<'ctx>,
}

impl<'a, 'p, 'ctx> FuncEmit<'a, 'p, 'ctx> {
    pub fn new(
        ctx: &'a Context<'a, 'ctx>, prog_emit: &'a ProgEmit<'p, 'ctx>, ir_func: &'a Function,
        ll_func: FunctionValue<'ctx>,
    ) -> Self {
        Self {
            ctx,
            ir_func,
            prog_emit,

            var_allocs: IndexVec::new(),
            locals: IndexVec::new(),
            jumper: None,
            blocks: IndexVec::new(),
            ll_func,
        }
    }
}

impl<'a, 'p, 'ctx> WalkActor for FuncEmit<'a, 'p, 'ctx> {
    fn pre_start(&mut self, _: &CFGWalker) {
        let entry_bb = self.emit_entry_block();
        for cfg_block in self.ir_func.blocks.iter() {
            let block = self.ctx.llvm_ctx.append_basic_block(
                self.ll_func,
                &format!("s{}b{}", cfg_block.scope.index(), cfg_block.id.index()),
            );
            self.blocks.push(block);
        }
        self.ctx.builder.position_at_end(entry_bb);
        self.jumper = Some(
            self.ctx
                .builder
                .build_unconditional_branch(self.blocks[BlockId::new(0)]),
        );
    }

    fn block_start(&mut self, _: &CFGWalker, id: BlockId) {
        let ll_block = self.blocks[id];
        self.ctx.builder.position_at_end(ll_block);
    }

    fn block_end(&mut self, _: &CFGWalker, id: BlockId) {
        self.emit_terminator(id);
    }

    fn handle_op(&mut self, walker: &CFGWalker, id: BlockId, op: &Op, idx: usize) {
        self.emit_op(walker, id, op, idx);
    }

    fn end(&mut self, _: &CFGWalker) {}
}

impl<'a, 'p, 'ctx> FuncEmit<'a, 'p, 'ctx> {
    fn get_ty(&self, ty: TypeId) -> &Type {
        self.prog_emit.env.types.get(ty)
    }

    fn build_alloca(&self, ty: impl BasicType<'ctx> + Copy, name: &str) -> PointerValue<'ctx> {
        match self.jumper {
            // Not set- we're still in setup + in the entry block, safe to just emit
            None => self.ctx.builder.build_alloca(ty, name),
            Some(jumper) => {
                let cur_block = self.ctx.builder.get_insert_block().unwrap();
                self.ctx.builder.position_before(&jumper);
                let alloca = self.ctx.builder.build_alloca(ty, name);
                self.ctx.builder.position_at_end(cur_block);
                alloca
            },
        }
    }

    fn build_gc_alloca(&self, ty: impl BasicType<'ctx> + Copy, name: &str) -> PointerValue<'ctx> {
        let alloca = self.build_alloca(ty, name);
        self.ctx
            .builder
            .build_address_space_cast(alloca, ty.ptr_type(GC_ADDRESS_SPACE), "")
    }

    fn build_val_alloca(&self, ty: &Type) -> PointerValue<'ctx> {
        match ty {
            Type::Float => self.build_gc_alloca(self.ctx.llvm_ctx.f32_type(), ""),
            Type::Ref(RefType::String) => self.build_gc_alloca(self.ctx.rt.ty.string_type_ptr, ""),
            Type::Ref(_) => self.build_gc_alloca(self.ctx.rt.ty.datum_common_type_ptr, ""),
            Type::Null => self.build_gc_alloca(self.ctx.llvm_ctx.i64_type(), ""),
            Type::Any | Type::OneOf(_) => self.build_gc_alloca(self.ctx.rt.ty.val_type, ""),
            _ => unimplemented!("unhandled ty: {:?}", ty),
        }
    }

    fn emit_entry_block(&mut self) -> BasicBlock<'ctx> {
        let block = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "entry");
        self.ctx.builder.position_at_end(block);

        for _ in 0..self.ir_func.locals.len() {
            self.locals.push(None);
        }

        for var in self.ir_func.vars.iter() {
            let name = &if var.name.index() != 0 {
                format!("var_{}", self.prog_emit.env.string_table.get(var.name))
            } else {
                "ret_var".to_string()
            };

            let alloca = self.build_val_alloca(self.get_ty(var.ty));
            alloca.set_name(name);
            self.var_allocs.push(StackValue::new(alloca, var.ty));
        }

        // TODO: Bring Back SSAValue
        self.build_var_zeroinit(0.into());

        self.emit_prelude();

        block
    }

    fn emit_prelude(&mut self) {
        // TODO: make calling conventions clearer in IR
        if let Some(closure) = &self.ir_func.closure {
            let mut cvars = closure.captured.clone();
            cvars.sort_unstable_by_key(|(_, id)| *id);
            let env_arg = self.ll_func.get_params()[0].into_pointer_value();
            for (i, (var_id, _)) in cvars.into_iter().enumerate() {
                let ptr = unsafe { self.const_gep(env_arg, &[i as u64]) };
                self.copy_val(ptr, self.var_allocs[var_id].val);
            }
        } else {
            let param_locals_arr = self.ctx.builder.build_array_alloca(
                self.ctx.rt.ty.val_type_ptr,
                self.ctx
                    .llvm_ctx
                    .i64_type()
                    .const_int(self.ir_func.params.len() as u64, false),
                "param_locals",
            );

            for (i, var_id) in self.ir_func.params.iter().enumerate() {
                let alloc = &self.var_allocs[*var_id];
                // TODO: handle keyword args
                unsafe {
                    self.const_gep_insertvalue(param_locals_arr, &[i as u64], alloc.val.into());
                }
            }

            let argpack_local = self.ll_func.get_params()[0];
            self.build_call(
                self.ctx.rt.rt_arg_pack_unpack_into,
                &[
                    argpack_local.into(),
                    param_locals_arr.into(),
                    self.ctx
                        .llvm_ctx
                        .i64_type()
                        .const_int(self.ir_func.id.index() as u64, false)
                        .into(),
                    self.prog_emit.rt_global.into(),
                ],
            );
        }
    }

    fn finalize_entry_block(&self, entry: BasicBlock) {
        self.ctx.builder.position_at_end(entry);
        self.ctx
            .builder
            .build_unconditional_branch(self.blocks[BlockId::new(0)]);
    }

    fn assign_literal(&mut self, lit: Literal, local_id: LocalId) {
        let val = self.build_literal(lit);
        self.set_local(local_id, val)
    }

    fn build_literal(&self, lit: Literal) -> BasicValueEnum<'ctx> {
        let val: BasicValueEnum = match lit {
            Literal::Num(x) => self.ctx.llvm_ctx.f32_type().const_float(x as f64).into(),
            Literal::String(id) => self.prog_emit.string_allocs[id].into(),
            Literal::Null => self.ctx.llvm_ctx.i64_type().const_zero().into(),
            _ => unimplemented!("{:?}", lit),
        };
        val
    }

    fn get_zero_value(&self, layout: ScalarLayout) -> Literal {
        match layout {
            ScalarLayout::Null => Literal::Null,
            ScalarLayout::Float => Literal::Num(0.0),
        }
    }

    fn get_local(&self, local: LocalId) -> BasicValueEnum<'ctx> {
        self.locals[local].unwrap_or_else(|| panic!("could not load local {:?}", local))
    }

    fn conv_any_into(
        &self, src_local: BasicValueEnum<'ctx>, src_ty: &Type, dest: PointerValue<'ctx>,
    ) {
        let (disc, llval, llval2) = match src_ty.get_layout() {
            Layout::Val => {
                // Special case: can just memcpy directly
                self.copy_val(src_local.into_pointer_value(), dest);
                return;
            },
            Layout::SoftRef => {
                // Special case: need to update two fields
                let vptr =
                    unsafe { self.const_gep_extractvalue(src_local.into_pointer_value(), &[0, 0]) };
                let ptr =
                    unsafe { self.const_gep_extractvalue(src_local.into_pointer_value(), &[0, 1]) };

                let vptr_int = self.ctx.builder.build_ptr_to_int(
                    vptr.into_pointer_value(),
                    self.ctx.llvm_ctx.i64_type(),
                    "vptr",
                );
                let ptr_int = self.ctx.builder.build_ptr_to_int(
                    ptr.into_pointer_value(),
                    self.ctx.llvm_ctx.i64_type(),
                    "ptr",
                );

                (layout::VAL_DISCRIM_REF, vptr_int, Some(ptr_int))
            },

            Layout::HardRef(RefType::String) => {
                let ptr_as_int = self.ctx.builder.build_ptr_to_int(
                    src_local.into_pointer_value(),
                    self.ctx.llvm_ctx.i64_type(),
                    "pack_val",
                );
                (layout::VAL_DISCRIM_STRING, ptr_as_int, None)
            },
            Layout::HardRef(_) => {
                let ptr_as_int = self.ctx.builder.build_ptr_to_int(
                    src_local.into_pointer_value(),
                    self.ctx.llvm_ctx.i64_type(),
                    "pack_val",
                );
                (layout::VAL_DISCRIM_REF, ptr_as_int, None)
            },
            Layout::Scalar(ScalarLayout::Null) => {
                let null_val = self.ctx.llvm_ctx.i64_type().const_zero();
                (layout::VAL_DISCRIM_NULL, null_val, None)
            },
            Layout::Scalar(ScalarLayout::Float) => {
                // TODO fix this, this is mega bad, won't work on big endian systems
                let val_as_i32 = self.ctx.builder.build_bitcast(
                    src_local,
                    self.ctx.llvm_ctx.i32_type(),
                    "pack_val",
                );
                let val_as_i64: IntValue = self.ctx.builder.build_int_z_extend(
                    val_as_i32.into_int_value(),
                    self.ctx.llvm_ctx.i64_type(),
                    "pack_val",
                );
                (layout::VAL_DISCRIM_FLOAT, val_as_i64, None)
            },
        };
        assert_eq!(llval.get_type().get_bit_width(), 64);

        let disc_val = self.ctx.llvm_ctx.i8_type().const_int(disc as u64, false);
        unsafe {
            self.const_gep_insertvalue(dest, &[0, 0], disc_val.into());
            self.const_gep_insertvalue(dest, &[0, 1, 0], llval.into());
            if let Some(llval2) = llval2 {
                self.const_gep_insertvalue(dest, &[0, 1, 1], llval.into());
            }
        }
    }

    fn conv_local_any_into(&self, local_id: LocalId, into: PointerValue<'ctx>) {
        let local = self.locals[local_id].unwrap();
        let ty = self.get_ty(self.ir_func.locals[local_id].ty);
        self.conv_any_into(local, ty, into);
    }

    fn get_local_any_ro(&self, local_id: LocalId) -> PointerValue<'ctx> {
        let local = self.locals[local_id].unwrap();
        let ty = self.get_ty(self.ir_func.locals[local_id].ty);

        // local already points to a val-layout struct
        if let Layout::Val = ty.get_layout() {
            return local.into_pointer_value();
        }

        // TODO: this probably doesn't need to be GC
        // it's required for now, since runtime funcs might be taking an actual stack GC val
        // maybe could addrspacecast them at the last minute?
        let reified_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
        self.conv_local_any_into(local_id, reified_alloca);
        reified_alloca
    }

    fn set_local(&mut self, local_id: LocalId, val: BasicValueEnum<'ctx>) {
        assert!(self.locals[local_id].is_none());

        // TODO: do we need lifetimes on locals?
        /*
        let lts_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.start", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lts_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.locals[local_id].ty.get_store_size(), false).into(), stackptr.into()]);
        */

        val.set_name(&format!("local_{}", local_id.index()));
        self.locals[local_id] = Some(val);
    }

    fn local_die(&self, _local_id: LocalId) {
        // TODO: do we need lifetimes on locals?
        /*
        let lte_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.end", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lte_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.locals[local_id].ty.get_store_size(), false).into(), stackptr.into()]);
        */
    }

    fn var_die(&self, _var_id: VarId) {
        /*
        let stackptr = self.var_allocs[var_id].val.unwrap();
        let lte_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.end", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lte_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.var_allocs[var_id].ty.get_store_size(), false).into(), stackptr.into()]);*/
    }

    /*
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
    */

    /*
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
    */

    fn copy_val(&self, src: PointerValue<'ctx>, dest: PointerValue<'ctx>) {
        self.prog_emit.copy_val(src, dest);
    }

    fn build_memcpy(&self, src: PointerValue<'ctx>, dest: PointerValue<'ctx>) {}

    fn build_call_internal<F>(
        &self, func: F, args: &[BveWrapper<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>>,
    {
        let args_ll: Vec<_> = args.iter().map(|arg| arg.bve).collect();
        self.ctx
            .builder
            .build_call(func, &args_ll, "")
            .try_as_basic_value()
            .left()
    }

    fn build_call<F>(&self, func: F, args: &[BveWrapper<'ctx>]) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>>,
    {
        let fptr = match func.into() {
            either::Either::Left(func_val) => func_val.as_global_value().as_pointer_value(),
            either::Either::Right(pointer_val) => pointer_val,
        };

        //self.build_call_statepoint(fptr, args)
        self.build_call_internal(fptr, &args)
    }

    fn build_call_statepoint(
        &self, block: &Block, live: &Lifetimes, func: PointerValue<'ctx>, args: &[BveWrapper<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>> {
        // TODO: NEEDS TO USE OP BUNDLES FOR LLVM 12+

        let sp_intrinsic = unsafe {
            self.ctx
                .module
                .get_intrinsic("llvm.experimental.gc.statepoint", &[func.get_type().into()])
                .unwrap()
        };
        let args_ll: Vec<_> = args.iter().map(|arg| arg.bve).collect();

        let mut stack_ptrs: Vec<BasicValueEnum> = Vec::new();
        for live_local in live.local.iter().copied() {
            if self.locals[live_local].unwrap().is_pointer_value() {
                let local_ty_id = self.ir_func.locals[live_local].ty;
                stack_ptrs.push(self.locals[live_local].unwrap());
                stack_ptrs.push(
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(local_ty_id.raw() as _, false)
                        .into(),
                );
            }
        }
        for live_var in live.var.iter().copied() {
            let var_ty_id = self.ir_func.vars[live_var].ty;
            let var_ty = self.prog_emit.env.types.get(var_ty_id);
            if var_ty.get_layout().is_val() {
                stack_ptrs.push(self.var_allocs[live_var].val.into());
                stack_ptrs.push(
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(var_ty_id.raw() as _, false)
                        .into(),
                );
            }
        }

        let mut sp_args = Vec::with_capacity(7 + args.len() + stack_ptrs.len() * 2);
        sp_args.extend(
            [
                self.ctx
                    .llvm_ctx
                    .i64_type()
                    .const_int(caer_types::layout::GC_STACKMAP_ID, false)
                    .into(), // id
                self.ctx.llvm_ctx.i32_type().const_zero().into(), // # patch bytes
                func.into(),                                      // func
                self.ctx
                    .llvm_ctx
                    .i32_type()
                    .const_int(args.len() as u64, false)
                    .into(), // # call args
                self.ctx.llvm_ctx.i32_type().const_zero().into(), // flags
            ]
            .iter(),
        );
        sp_args.extend(args_ll.into_iter());
        sp_args.extend(
            [
                self.ctx.llvm_ctx.i32_type().const_zero().into(), // # transition args
                //self.ctx.llvm_ctx.i32_type().const_zero().into(), // # deopt args
                self.ctx
                    .llvm_ctx
                    .i32_type()
                    .const_int(stack_ptrs.len() as u64, false)
                    .into(), // # deopt args
            ]
            .iter(),
        );
        sp_args.extend(stack_ptrs.into_iter());

        let statepoint_token;
        if let Some(pad) = self.ir_func.scopes[block.scope].landingpad {
            let continuation = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "");
            statepoint_token = self
                .ctx
                .builder
                .build_invoke(sp_intrinsic, &sp_args, continuation, self.blocks[pad], "")
                .try_as_basic_value()
                .left()
                .unwrap();
            self.ctx.builder.position_at_end(continuation);
        } else {
            statepoint_token = self
                .ctx
                .builder
                .build_call(sp_intrinsic, &sp_args, "")
                .try_as_basic_value()
                .left()
                .unwrap();
        }

        let return_type = func
            .get_type()
            .get_element_type()
            .into_function_type()
            .get_return_type();
        if let Some(return_type) = return_type {
            let result_intrinsic = unsafe {
                self.ctx
                    .module
                    .get_intrinsic("llvm.experimental.gc.result", &[return_type])
                    .unwrap()
            };
            self.ctx
                .builder
                .build_call(result_intrinsic, &[statepoint_token], "")
                .try_as_basic_value()
                .left()
        } else {
            None
        }
    }

    fn build_call_intrinsic(
        &mut self, intrinsic: Intrinsic, args: &[BveWrapper<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>> {
        let func = self.prog_emit.get_intrinsic(intrinsic);
        self.build_call_internal(func, args)
    }

    fn build_call_catching<F>(
        &self, block: &Block, func: F, args: &[BveWrapper<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>>,
    {
        if let Some(pad) = self.ir_func.scopes[block.scope].landingpad {
            let args_ll: Vec<_> = args.iter().map(|arg| arg.bve).collect();
            let continuation = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "");
            let res = self
                .ctx
                .builder
                .build_invoke(func, &args_ll, continuation, self.blocks[pad], "")
                .try_as_basic_value()
                .left();
            self.ctx.builder.position_at_end(continuation);
            res
        } else {
            self.build_call_internal(func, args)
        }
    }

    fn emit_op(&mut self, walker: &CFGWalker, id: BlockId, op: &Op, idx: usize) {
        let block = &self.ir_func.blocks[id];
        match op {
            Op::Noop => {},

            Op::Literal(id, literal) => {
                self.assign_literal(*literal, *id);
            },

            Op::MkVar(_) => {
                // nothing now - can we remove this?
            },

            Op::Load(local, var) => {
                let local_ty = self.get_ty(self.ir_func.locals[*local].ty);
                let var_ty = self.get_ty(self.ir_func.vars[*var].ty);

                let local_val = match (var_ty.get_layout(), local_ty.get_layout()) {
                    // Val into val OR soft ref into soft ref: alloc and memcpy
                    (Layout::Val, Layout::Val) | (Layout::SoftRef, Layout::SoftRef) => {
                        let copied_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                        self.copy_val(self.var_allocs[*var].val, copied_alloca);
                        copied_alloca.into()
                    },

                    // Anything else into val: alloc, tag, pack
                    (_, Layout::Val) => {
                        todo!()
                    },

                    // Val into anything else: impossible
                    (Layout::Val, other) => panic!(
                        "tryng to load val-layout var into local with layout {:?}",
                        other
                    ),

                    // Hard ref into soft ref: alloc and pack
                    (Layout::HardRef(_), Layout::SoftRef) => {
                        todo!()
                    },

                    // Soft ref into anything else: impossible
                    (Layout::SoftRef, other) => panic!(
                        "trying to load soft-ref var into local with layout {:?}",
                        other
                    ),

                    // Anything else into soft ref: impossible
                    // TODO: nulls?
                    (other, Layout::SoftRef) => panic!(
                        "trying to load var with layout {:?} into soft-ref local",
                        other
                    ),

                    // Word-sized types: simple load if compatible
                    (
                        lhs @ (Layout::HardRef(_) | Layout::Scalar(_)),
                        rhs @ (Layout::HardRef(_) | Layout::Scalar(_)),
                    ) => {
                        // TODO: better compatibility check, allow coercion? nulls?
                        // TODO: ERRH
                        if lhs != rhs {
                            panic!(
                                "word-size layouts are incompatible: trying to load {:?} into {:?}",
                                lhs, rhs
                            );
                        }
                        self.ctx.builder.build_load(self.var_allocs[*var].val, "")
                    },
                };

                /*if var_ty.get_layout().is_val() {
                    assert!(
                        local_ty.get_layout().is_val(),
                        "trying to load val-layout var into non-val local"
                    );
                    // var points to reified val struct, so memcpy
                    let copied_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                    self.copy_val(self.var_allocs[*var].val, copied_alloca);
                    copied_alloca.into();
                } else {
                    assert!(!local_ty.get_layout().is_val());
                }

                let val = if var_ty.is_any() {
                } else {
                    assert!(!local_ty.is_any());
                    self.ctx.builder.build_load(self.var_allocs[*var].val, "")
                };*/
                self.set_local(*local, local_val);
            },

            Op::Store(var, local) => {
                let var_ty = self.get_ty(self.ir_func.vars[*var].ty);
                let local_ty = self.get_ty(self.ir_func.locals[*local].ty);

                if var_ty.get_layout().is_val() {
                    self.conv_local_any_into(*local, self.var_allocs[*var].val);
                } else {
                    // trust that types are compatible
                    // tyinfer wouldn't lie to us
                    assert!(local_ty == var_ty);

                    let val = self.get_local(*local);
                    self.ctx.builder.build_store(self.var_allocs[*var].val, val);
                };
            },

            Op::Put(id) => {
                let norm = self.get_local_any_ro(*id);

                self.build_call_catching(
                    block,
                    self.ctx.rt.rt_val_print,
                    &[norm.into(), self.prog_emit.rt_global.into()],
                );
            },

            Op::Binary(id, op, lhs, rhs) => {
                let lhs_ref = self.get_local_any_ro(*lhs);
                let rhs_ref = self.get_local_any_ro(*rhs);
                let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false);

                let res = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                self.build_call_catching(
                    block,
                    self.ctx.rt.rt_val_binary_op,
                    &[
                        self.prog_emit.rt_global.into(),
                        op_var.into(),
                        lhs_ref.into(),
                        rhs_ref.into(),
                        res.into(),
                    ],
                );
                self.set_local(*id, res.into());
            },

            Op::HardBinary(id, op, lhs, rhs) => {
                let lhs_val = self.get_local(*lhs);
                let rhs_val = self.get_local(*rhs);
                let res = self.emit_binary(block, *op, lhs_val, rhs_val);
                self.set_local(*id, res);
            },

            Op::Call(id, name, args) => {
                let argpack_ptr = self.build_argpack(None, args);

                // TODO: better proc lookup, consider src
                let func = self.prog_emit.sym[self.prog_emit.lookup_global_proc(*name)]
                    .as_global_value()
                    .as_pointer_value();
                let res_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                self.build_call_statepoint(
                    block,
                    &walker.get_cur_lifetimes(),
                    func,
                    &[
                        argpack_ptr.into(),
                        self.prog_emit.rt_global.into(),
                        res_alloca.into(),
                    ],
                );
                self.set_local(*id, res_alloca.into());
            },

            Op::Cast(dst, src, ty_id) => {
                let ty = self.get_ty(*ty_id);
                assert_eq!(self.ir_func.locals[*dst].ty, *ty_id);
                if *ty != Type::Ref(RefType::String) {
                    unimplemented!("can only cast to string, not {:?}", ty);
                }

                let src_llval = self.get_local_any_ro(*src);
                let res_llval = self
                    .build_call_catching(
                        block,
                        self.ctx.rt.rt_val_cast_string_val,
                        &[src_llval.into(), self.prog_emit.rt_global.into()],
                    )
                    .unwrap();
                self.set_local(*dst, res_llval);
            },

            Op::AllocDatum(dst, ity_id) => {
                // In future, AllocDatum will take a val for the type ID.
                let ty_id_val = self
                    .ctx
                    .llvm_ctx
                    .i32_type()
                    .const_int(ity_id.raw() as u64, false);
                let vt_ptr = unsafe {
                    self.ctx.builder.build_in_bounds_gep(
                        self.prog_emit.vt_global.as_pointer_value(),
                        &[self.ctx.llvm_ctx.i32_type().const_zero(), ty_id_val],
                        "vt_ptr",
                    )
                };

                // TODO: impl eq for prim<->complex
                // TODO: assert actual type once inference pops it
                let ty = self.get_ty(self.ir_func.locals[*dst].ty);
                assert_eq!(*ty, RefType::Any.into());
                // Currently, all alloc vals are the "anyref" kind, i.e. P::Ref(None).
                // Therefore, we'll always store a fat pointer in dst
                // In future it might be worth making anyref its own primitive type?
                // Possibly split into 3:
                // - Ref - nonnull, exact type, thin pointer
                // - SubRef - nonnull, exact pathtype or subtype thereof, fat pointer
                // - AnyRef - maybe equivalent to SubRef(/datum)? idk. fat pointer
                // Not sure how this would interact with generics: TODO(generics)
                let datum_ptr = self
                    .build_call_catching(
                        block,
                        self.ctx.rt.rt_runtime_alloc_datum,
                        &[
                            self.prog_emit.rt_global.into(),
                            self.ctx
                                .llvm_ctx
                                .i32_type()
                                .const_int(ity_id.index() as u64, false)
                                .into(),
                        ],
                    )
                    .unwrap();

                let ref_ptr = self.build_alloca(self.ctx.rt.ty.ref_type, "ref_ptr");
                unsafe { self.const_gep_insertvalue(ref_ptr, &[0, 0], vt_ptr.into()) };
                unsafe { self.const_gep_insertvalue(ref_ptr, &[0, 1], datum_ptr) };

                // TODO: embed datum ty info
                self.set_local(*dst, ref_ptr.into());
            },

            Op::DatumLoadVar(dst, src, var_id) => {
                assert!(self
                    .get_ty(self.ir_func.locals[*dst].ty)
                    .get_layout()
                    .is_val());

                let rval_ptr = self.get_local_any_ro(*src);
                let (ref_vptr, ref_ptr) = self.build_extract_ref_ptrs(rval_ptr);
                let var_get_ptr = self
                    .build_vtentry_extract(ref_vptr, layout::VTABLE_VAR_GET_FIELD_OFFSET)
                    .into_pointer_value();
                dbg!(rval_ptr, ref_ptr);

                // TODO: drop/cloned

                dbg!(var_get_ptr);
                let loaded_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                self.build_call_catching(
                    block,
                    var_get_ptr,
                    &[
                        ref_ptr.into(),
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(var_id.index() as u64, false)
                            .into(),
                        loaded_alloca.into(),
                    ],
                );
                self.set_local(*dst, loaded_alloca.into());
            },

            Op::DatumStoreVar(dst, var_id, src) => {
                let lval_ptr = self.get_local_any_ro(*dst);
                let (ref_vptr, ref_ptr) = self.build_extract_ref_ptrs(lval_ptr);
                let var_set_ptr = self
                    .build_vtentry_extract(ref_vptr, layout::VTABLE_VAR_SET_FIELD_OFFSET)
                    .into_pointer_value();

                let src_val = self.get_local_any_ro(*src);
                self.build_call_catching(
                    block,
                    var_set_ptr,
                    &[
                        ref_ptr.into(),
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(var_id.index() as u64, false)
                            .into(),
                        src_val.into(),
                    ],
                );
            },

            Op::DatumCallProc(dst, src, proc_name, args) => {
                todo!("very broken");
                let argpack_ptr = self.build_argpack(Some(*src), args);
                let _src_val = self.get_local(*src);
                let src_val_any = self.get_local_any_ro(*src);
                match self.get_ty(self.ir_func.locals[*src].ty) {
                    Type::Ref(_) => {
                        let (ref_vptr, ref_ptr) = self.build_extract_ref_ptrs(src_val_any);
                        let proc_lookup_ptr = self
                            .build_vtentry_extract(
                                ref_vptr,
                                layout::VTABLE_PROC_LOOKUP_FIELD_OFFSET,
                            )
                            .into_pointer_value();

                        let proc_ptr = self
                            .build_call_statepoint(
                                block,
                                &walker.get_cur_lifetimes(),
                                proc_lookup_ptr,
                                &[
                                    self.ctx
                                        .llvm_ctx
                                        .i64_type()
                                        .const_int(proc_name.index() as u64, false)
                                        .into(),
                                    self.prog_emit.rt_global.into(),
                                ],
                            )
                            .unwrap()
                            .into_pointer_value();

                        let res_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                        self.build_call_catching(
                            block,
                            proc_ptr,
                            &[
                                argpack_ptr.into(),
                                self.prog_emit.rt_global.into(),
                                res_alloca.into(),
                            ],
                        );

                        self.set_local(*dst, res_alloca.into());
                    },
                    Type::Ref(RefType::String) | _ => {
                        // soft call
                        let res_alloca = self.build_gc_alloca(self.ctx.rt.ty.val_type, "");
                        self.build_call_statepoint(
                            block,
                            &walker.get_cur_lifetimes(),
                            self.ctx
                                .rt
                                .rt_val_call_proc
                                .as_global_value()
                                .as_pointer_value(),
                            &[
                                src_val_any.into(),
                                self.ctx
                                    .llvm_ctx
                                    .i64_type()
                                    .const_int(proc_name.index() as u64, false)
                                    .into(),
                                argpack_ptr.into(),
                                self.prog_emit.rt_global.into(),
                                res_alloca.into(),
                            ],
                        );

                        self.set_local(*dst, res_alloca.into());
                    },
                }
            },

            Op::Throw(exception_local) => {
                let exception_val = self.get_local_any_ro(*exception_local);
                self.build_call_catching(block, self.ctx.rt.rt_throw, &[exception_val.into()]);
            },

            Op::CatchException(maybe_except_var) => {
                if idx != 0 {
                    panic!("CatchException op not at start of block");
                }

                let landingpad = self
                    .ctx
                    .builder
                    .build_landingpad(self.ctx.rt.ty.landingpad_type, "lp");
                landingpad.set_cleanup(true);

                if let Some(except_var) = maybe_except_var {
                    let exception_container = self
                        .ctx
                        .builder
                        .build_extract_value(landingpad.as_basic_value().into_struct_value(), 0, "")
                        .unwrap();
                    assert!(self
                        .get_ty(self.ir_func.vars[*except_var].ty)
                        .get_layout()
                        .is_val());
                    self.build_call(
                        self.ctx.rt.rt_exception_get_val,
                        &[
                            exception_container.into(),
                            self.var_allocs[*except_var].val.into(),
                        ],
                    );
                }
            },

            Op::Spawn(closure_slot, delay) => {
                assert!(delay.is_none());
                println!("in spawn");

                let func_id = self.ir_func.child_closures[*closure_slot];
                let cv = self.gather_captured_vars(func_id);

                println!("cv: {:?}", cv);

                // For now, all closure vars are soft/Any.
                // TODO: support types on closure boundary

                let cap_array = self.ctx.builder.build_array_alloca(
                    self.ctx.rt.ty.val_type,
                    self.ctx
                        .llvm_ctx
                        .i64_type()
                        .const_int(cv.len() as u64, false),
                    "cap_array",
                );

                for (i, (my_var, _)) in cv.iter().copied().enumerate() {
                    let var_val = self.ctx.builder.build_load(self.var_allocs[my_var].val, "");
                    let ptr = unsafe { self.const_gep(cap_array, &[i as u64]) };
                    self.conv_any_into(var_val, self.get_ty(self.var_allocs[my_var].ty), ptr);
                }

                self.build_call_statepoint(
                    block,
                    &walker.get_cur_lifetimes(),
                    self.ctx
                        .rt
                        .rt_runtime_spawn_closure
                        .as_global_value()
                        .as_pointer_value(),
                    &[
                        self.prog_emit.rt_global.into(),
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(func_id.raw(), false)
                            .into(),
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(cv.len() as u64, false)
                            .into(),
                        // Is this even valid?
                        self.ctx
                            .builder
                            .build_address_space_cast(cap_array, self.ctx.rt.ty.val_type_ptr, "")
                            .into(),
                    ],
                );
            },

            Op::Sleep(delay) => {
                let local = self.get_local(*delay);
                println!("{:?}", walker.get_cur_lifetimes());
                self.build_call(
                    self.ctx
                        .rt
                        .rt_runtime_suspend
                        .as_global_value()
                        .as_pointer_value(),
                    &[
                        self.prog_emit.rt_global.as_pointer_value().into(),
                        local.into(),
                    ],
                );
            },
            //_ => unimplemented!("{:?}", op),
        }
    }

    fn emit_terminator(&mut self, block_id: BlockId) {
        let block = &self.ir_func.blocks[block_id];
        match &block.terminator {
            Terminator::Return => {
                self.finalize_block(block);
                // TODO: rewrite, kludge
                let ret_out = self.ll_func.get_params()[2].into_pointer_value();
                let ret_ty = self.get_ty(self.ir_func.vars[0].ty);
                if ret_ty.get_layout().is_val() {
                    self.copy_val(self.var_allocs[0].val, ret_out);
                } else {
                    let ret = self.ctx.builder.build_load(self.var_allocs[0].val, "");
                    self.conv_any_into(ret, ret_ty, ret_out);
                }
                self.ctx.builder.build_return(None);
            },

            Terminator::Jump(id) => {
                self.finalize_block(block);
                self.ctx
                    .builder
                    .build_unconditional_branch(self.blocks[*id]);
            },

            Terminator::Switch {
                discriminant,
                branches,
                default,
            } => {
                let disc_local = &self.ir_func.locals[*discriminant];

                let disc_int = match self.get_ty(disc_local.ty) {
                    Type::Null => self.ctx.llvm_ctx.i32_type().const_int(0, false),
                    Type::Float => {
                        let disc_val = self.get_local(*discriminant);
                        self.ctx
                            .builder
                            .build_cast(
                                InstructionOpcode::FPToSI,
                                disc_val,
                                self.ctx.llvm_ctx.i32_type(),
                                "disc",
                            )
                            .into_int_value()
                    },
                    Type::Any | Type::OneOf(_) => {
                        let disc_val = self.get_local_any_ro(*discriminant);
                        self.build_call_catching(
                            block,
                            self.ctx.rt.rt_val_to_switch_disc,
                            &[disc_val.into()],
                        )
                        .unwrap()
                        .into_int_value()
                    },
                    _ => unimplemented!("{:?}", disc_local.ty),
                };

                self.finalize_block(block);

                // convert branches to use llvm blocks
                let llvm_branches = branches
                    .iter()
                    .map(|(lit, target)| {
                        let val = self.ctx.llvm_ctx.i32_type().const_int((*lit).into(), false);
                        (val, self.blocks[*target])
                    })
                    .collect::<Vec<_>>();

                self.ctx
                    .builder
                    .build_switch(disc_int, self.blocks[*default], &llvm_branches[..]);
            },

            Terminator::TryCatch { try_block, .. } => {
                self.finalize_block(block);
                self.ctx
                    .builder
                    .build_unconditional_branch(self.blocks[*try_block]);
            },
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

    fn emit_binary(
        &mut self, block: &Block, op: op::HardBinary, lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let res = match op {
            op::HardBinary::StringConcat => self
                .build_call_catching(
                    block,
                    self.ctx.rt.rt_runtime_concat_strings,
                    &[self.prog_emit.rt_global.into(), lhs.into(), rhs.into()],
                )
                .unwrap(),
            op::HardBinary::FloatAdd => self
                .ctx
                .builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "")
                .into(),
            op::HardBinary::FloatSub => self
                .ctx
                .builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "")
                .into(),
            op::HardBinary::FloatMul => self
                .ctx
                .builder
                .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "")
                .into(),
            op::HardBinary::FloatDiv => self
                .ctx
                .builder
                .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "")
                .into(),
            op::HardBinary::FloatMod => self
                .ctx
                .builder
                .build_float_rem(lhs.into_float_value(), rhs.into_float_value(), "")
                .into(),
            op::HardBinary::FloatPow => self
                .build_call_intrinsic(Intrinsic::FPow, &[lhs.into(), rhs.into()])
                .unwrap(),
            op::HardBinary::FloatCmp(pred) => {
                let bool_res = self.ctx.builder.build_float_compare(
                    Self::conv_pred(pred),
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "",
                );
                self.ctx
                    .builder
                    .build_unsigned_int_to_float(bool_res, self.ctx.llvm_ctx.f32_type(), "")
                    .into()
            },
            op::HardBinary::FloatBitOp(bitop) => {
                let lhs_i24 = self.float_to_i24(lhs.into_float_value());
                let rhs_i24 = self.float_to_i24(rhs.into_float_value());
                let res_i24 = match bitop {
                    op::BitOp::And => self.ctx.builder.build_and(lhs_i24, rhs_i24, ""),
                    op::BitOp::Or => self.ctx.builder.build_or(lhs_i24, rhs_i24, ""),
                    op::BitOp::Xor => self.ctx.builder.build_xor(lhs_i24, rhs_i24, ""),
                    op::BitOp::Shl => self.ctx.builder.build_left_shift(lhs_i24, rhs_i24, ""),
                    op::BitOp::Shr => self
                        .ctx
                        .builder
                        .build_right_shift(lhs_i24, rhs_i24, false, ""),
                };
                self.i24_to_float(res_i24).into()
            },
        };
        res
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

        let f_min_val = self
            .ctx
            .builder
            .build_bitcast(i32_type.const_int(f_min_int.into(), false), f32_type, "")
            .into_float_value();
        let f_max_val = self
            .ctx
            .builder
            .build_bitcast(i32_type.const_int(f_max_int.into(), false), f32_type, "")
            .into_float_value();
        let f_trunc_val = self
            .ctx
            .builder
            .build_float_to_signed_int(f32_val, i32_type, "");

        let i_min_val = i32_type.const_int(i32::MIN as u64, true);
        let i_max_val = i32_type.const_int(i32::MAX as u64, true);

        let less_or_nan = self.ctx.builder.build_float_compare(
            inkwell::FloatPredicate::ULT,
            f32_val,
            f_min_val,
            "",
        );
        let greater = self.ctx.builder.build_float_compare(
            inkwell::FloatPredicate::OGT,
            f32_val,
            f_max_val,
            "",
        );
        let is_nan = self.ctx.builder.build_float_compare(
            inkwell::FloatPredicate::UNO,
            f32_val,
            f32_val,
            "",
        );

        let s0 = self
            .ctx
            .builder
            .build_select(less_or_nan, i_min_val, f_trunc_val, "");
        let s1 = self
            .ctx
            .builder
            .build_select(greater, i_max_val.into(), s0, "");
        self.ctx
            .builder
            .build_select(is_nan, i32_type.const_zero().into(), s1, "")
            .into_int_value()
    }

    fn float_to_i24(&self, f32_val: FloatValue<'ctx>) -> IntValue<'ctx> {
        let i24_type = self.ctx.llvm_ctx.custom_width_int_type(24);
        let i32_val = self.f32_to_i32_sat(f32_val);
        self.ctx.builder.build_int_truncate(i32_val, i24_type, "")
    }

    fn i24_to_float(&self, i24_val: IntValue<'ctx>) -> FloatValue<'ctx> {
        self.ctx
            .builder
            .build_unsigned_int_to_float(i24_val, self.ctx.llvm_ctx.f32_type(), "")
    }

    unsafe fn const_gep_insertvalue(
        &self, ptr: PointerValue<'ctx>, indexes: &[u64], val: BasicValueEnum<'ctx>,
    ) {
        self.ctx
            .builder
            .build_store(self.const_gep(ptr, indexes), val);
    }

    unsafe fn const_gep_extractvalue(
        &self, ptr: PointerValue<'ctx>, indexes: &[u64],
    ) -> BasicValueEnum<'ctx> {
        self.ctx
            .builder
            .build_load(self.const_gep(ptr, indexes), "")
    }

    unsafe fn const_gep(&self, ptr: PointerValue<'ctx>, indexes: &[u64]) -> PointerValue<'ctx> {
        let gep_indexes: Vec<_> = indexes
            .iter()
            .map(|i| self.ctx.llvm_ctx.i32_type().const_int(*i, false))
            .collect();
        self.ctx
            .builder
            .build_in_bounds_gep(ptr, &gep_indexes, "gepiv_ptr")
    }

    fn build_argpack(&self, src: Option<LocalId>, args: &[LocalId]) -> PointerValue<'ctx> {
        let n_val = self
            .ctx
            .llvm_ctx
            .i64_type()
            .const_int(args.len() as u64, false);
        let argpack_unnamed = self.ctx.builder.build_array_alloca(
            self.ctx.rt.ty.val_type,
            self.ctx
                .llvm_ctx
                .i64_type()
                .const_int(args.len() as u64, false),
            "unnamed_args",
        );
        for (i, arg) in args.iter().enumerate() {
            let ptr = unsafe { self.const_gep(argpack_unnamed, &[i as u64]) };
            self.conv_local_any_into(*arg, ptr);
        }

        let argpack = self.build_alloca(self.ctx.rt.ty.arg_pack_type, "argpack");
        unsafe {
            self.const_gep_insertvalue(argpack, &[0, 0], n_val.into());
            self.const_gep_insertvalue(argpack, &[0, 1], argpack_unnamed.into());
            self.const_gep_insertvalue(
                argpack,
                &[0, 2],
                self.ctx.llvm_ctx.i64_type().const_zero().into(),
            );
            if let Some(id) = src {
                let ptr = self.const_gep(argpack, &[0, 4]);
                self.conv_local_any_into(id, ptr);
            }
        }

        argpack
    }

    /// Returns (vptr, ptr).
    fn build_extract_ref_ptrs(
        &self, rval_ptr: PointerValue<'ctx>,
    ) -> (PointerValue<'ctx>, PointerValue<'ctx>) {
        // TODO: tycheck we're a ref
        // TODO: use some constant instead of 1
        // TODO: TYAPI
        // TODO: cast val struct instead of int2ptr
        println!("{:?}", rval_ptr);
        let val_vptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                rval_ptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx.llvm_ctx.i32_type().const_int(0, false),
                ],
                "val_vptr",
            )
        };
        let val_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                rval_ptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx.llvm_ctx.i32_type().const_int(1, false),
                ],
                "val_ptr",
            )
        };
        let ref_vptr_int = self
            .ctx
            .builder
            .build_load(val_vptr, "ref_vptr_int")
            .into_int_value();
        let ref_ptr_int = self
            .ctx
            .builder
            .build_load(val_ptr, "ref_ptr_int")
            .into_int_value();

        (
            self.ctx.builder.build_int_to_ptr(
                ref_vptr_int,
                self.ctx.rt.ty.vt_entry_type_ptr,
                "ref_vptr",
            ),
            self.ctx.builder.build_int_to_ptr(
                ref_ptr_int,
                self.ctx.rt.ty.datum_common_type_ptr,
                "ref_ptr",
            ),
        )
    }

    /*fn build_extract_ty_id(&self, datum_ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        let ty_id_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                datum_ptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(layout::DATUM_TY_FIELD_OFFSET, false),
                ],
                "ty_id_ptr",
            )
        };
        let ty_id = self
            .ctx
            .builder
            .build_load(ty_id_ptr, "ty_id")
            .into_int_value();
        ty_id
    }*/

    fn build_vtable_lookup_inline(
        &self, ty_id: IntValue<'ctx>, offset: u64,
    ) -> BasicValueEnum<'ctx> {
        let field_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                self.prog_emit.vt_global.as_pointer_value(),
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    ty_id,
                    self.ctx.llvm_ctx.i32_type().const_int(offset, false),
                ],
                &format!("vtable_field_{}_ptr", offset),
            )
        };

        self.ctx
            .builder
            .build_load(field_ptr, &format!("vtable_field_{}", offset))
    }

    fn build_vtentry_extract(&self, vptr: PointerValue<'ctx>, offset: u64) -> BasicValueEnum<'ctx> {
        let field_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                vptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx.llvm_ctx.i32_type().const_int(offset, false),
                ],
                &format!("vtable_field_{}_ptr", offset),
            )
        };

        self.ctx
            .builder
            .build_load(field_ptr, &format!("vtable_field_{}", offset))
    }

    fn build_var_zeroinit(&self, var: VarId) {
        let var_ty = self.get_ty(self.ir_func.vars[var].ty);
        let val = self.var_allocs[var].val;

        match var_ty.get_layout() {
            Layout::Val => {
                let null_disc = self
                    .ctx
                    .llvm_ctx
                    .i8_type()
                    .const_int(layout::VAL_DISCRIM_NULL as _, false);
                unsafe { self.const_gep_insertvalue(val, &[0, 0], null_disc.into()) };
            },
            Layout::SoftRef => todo!("can't yet zeroinit virtual nulls"),
            Layout::HardRef(_) => {
                let null_ptr = val.get_type().const_null();
                self.ctx.builder.build_store(val, null_ptr);
            },
            Layout::Scalar(scalar_layout) => {
                let scalar_zero = self.get_zero_value(scalar_layout);
                self.ctx
                    .builder
                    .build_store(val, self.build_literal(scalar_zero));
            },
        }
    }

    /*fn build_vtable_lookup(
        &self, block: &Block, datum_ptr: PointerValue<'ctx>, offset: u64,
    ) -> BasicValueEnum<'ctx> {
        let lookup_fn = self.prog_emit.vt_lookup[offset as usize];
        let field = self
            .build_call_catching(block, lookup_fn, &[datum_ptr.into()])
            .unwrap();
        field.set_name(&format!("vtable_field_{}", offset));
        field
    }*/

    fn gather_captured_vars(&self, closure: FuncId) -> Vec<(VarId, VarId)> {
        let mut vars = Vec::new();

        for var in self.ir_func.vars.iter() {
            if let Some(closure_var) = var.captures.get(&closure) {
                vars.push((var.id, *closure_var));
            }
        }

        vars.sort_unstable_by_key(|(_, id)| *id);
        vars
    }

    fn finalize_block(&self, block: &Block) {
        if block.scope_end {
            for local_id in self.ir_func.scopes[block.scope].destruct_locals.iter() {
                let local = &self.ir_func.locals[*local_id];
                /*if local.ty.needs_destructor() {
                    self.build_call(
                        self.ctx.rt.rt_val_drop,
                        &[self.get_local_any_ro(*local_id).into()],
                    );
                }*/
                self.local_die(*local_id);
            }
            for var_id in self.ir_func.scopes[block.scope].destruct_vars.iter() {
                self.var_die(*var_id);
            }
        }
    }
}
