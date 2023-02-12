use std::ptr::NonNull;

use caer_ir::cfg::*;
use caer_ir::id::{BlockId, LocalId, VarId};
use caer_ir::walker::{CFGWalker, Lifetimes, WalkActor};
use caer_runtime::arg_pack::ProcPack;
use caer_runtime::datum::Datum;
use caer_runtime::ffi::FfiArray;
use caer_runtime::heap_object::HeapHeader;
use caer_runtime::rtti::RttiRef;
use caer_runtime::string::RtString;
use caer_runtime::val::Val;
use caer_runtime::vtable::Entry;
use caer_types::id::{FuncId, StringId, TypeId};
use caer_types::op::BinaryOp;
use caer_types::ty::{self, Layout, RefType, ScalarLayout};
use caer_types::{layout, op};
use index_vec::IndexVec;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicType;
use inkwell::values::*;
use pinion::{PinionData, PinionPointerType, PinionStruct};
use ty::Type;

use crate::context::{Context, ExFunc, ExMod, GC_ADDRESS_SPACE};
use crate::prog::{Intrinsic, ProgEmit};
use crate::value::{BrandedStackValue, BrandedValue};

#[derive(Debug)]
pub struct FuncEmit<'a, 'p, 'ctx> {
    ctx: &'a Context<'a, 'ctx>,
    ir_func: &'a Function,
    prog_emit: &'a ProgEmit<'p, 'ctx>,

    var_allocs: IndexVec<VarId, BrandedStackValue<'ctx>>,
    locals: IndexVec<LocalId, Option<BrandedStackValue<'ctx>>>,
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

    fn build_val_alloca(&self, ty: &Type) -> BrandedStackValue<'ctx> {
        match ty {
            Type::Float => BrandedValue::<*mut f32>::build_as_alloca(self.ctx).into(),
            Type::Ref(RefType::String) => {
                BrandedValue::<*mut Option<NonNull<RtString>>>::build_as_alloca(self.ctx).into()
            },
            Type::Ref(_) => BrandedValue::<*mut RttiRef>::build_as_alloca(self.ctx).into(),
            Type::Null => BrandedStackValue::Null,
            Type::Any | Type::OneOf(_) => {
                BrandedValue::<*mut Val>::build_as_alloca(self.ctx).into()
            },
            /*Type::Float => self.build_gc_alloca(self.ctx.llvm_ctx.f32_type(), ""),
            Type::Ref(RefType::String) => self.build_gc_alloca(self.ctx.get_type::<RtString>(), ""),
            Type::Ref(_) => self.build_gc_alloca(self.ctx.get_type::<RttiRef>(), ""),
            Type::Null => self.build_gc_alloca(self.ctx.llvm_ctx.i64_type(), ""),
            Type::Any | Type::OneOf(_) => self.build_gc_alloca(self.ctx.get_enum::<Val>().ty, ""),*/
            //_ => unimplemented!("unhandled ty: {:?}", ty),
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
            self.var_allocs.push(alloca);
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
            todo!();
            /*for (i, (var_id, _)) in cvars.into_iter().enumerate() {
                let ptr = unsafe { self.const_gep(env_arg, &[i as u64]) };
                self.copy_val(ptr, self.var_allocs[var_id].val);
            }*/
        } else {
            let param_locals_arr = self.ctx.builder.build_array_alloca(
                self.ctx
                    .get_enum::<Val>()
                    .ty
                    .ptr_type(inkwell::AddressSpace::Generic),
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
                    self.const_gep_insertvalue(
                        param_locals_arr,
                        &[i as u64],
                        alloc.as_val().expect("arg var must be Val").val,
                    );
                }
            }

            let argpack_local = self.ll_func.get_params()[0];

            self.build_call(
                self.ctx.get_func(ExFunc::rt_arg_pack_unpack_into),
                &[
                    argpack_local.into(),
                    param_locals_arr.into(),
                    self.ctx
                        .llvm_ctx
                        .i64_type()
                        .const_int(self.ir_func.id.index() as u64, false)
                        .into(),
                    self.prog_emit.rt_global.as_basic_value_enum().into(),
                ],
            );
        }
    }

    /*fn finalize_entry_block(&self, entry: BasicBlock) {
        self.ctx.builder.position_at_end(entry);
        self.ctx
            .builder
            .build_unconditional_branch(self.blocks[BlockId::new(0)]);
    }*/

    fn assign_literal(&mut self, lit: Literal, local_id: LocalId) {
        let val = self.build_literal(lit);
        self.set_local(local_id, val)
    }

    fn build_literal(&self, lit: Literal) -> BrandedStackValue<'ctx> {
        match lit {
            Literal::Num(x) => {
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, BrandedValue::<f32>::literal(x, self.ctx));
                alloca.into()
            },
            Literal::String(id) => self.prog_emit.string_allocs[id]
                .alloca_emplace(self.ctx)
                .into(),
            Literal::Null => BrandedStackValue::Null,
            _ => unimplemented!("{:?}", lit),
        }
    }

    fn get_zero_value(&self, layout: ScalarLayout) -> Literal {
        match layout {
            ScalarLayout::Null => Literal::Null,
            ScalarLayout::Float => Literal::Num(0.0),
        }
    }

    fn get_local(&self, local: LocalId) -> BrandedStackValue<'ctx> {
        self.locals[local].unwrap_or_else(|| panic!("could not load local {:?}", local))
    }

    fn conv_any_into(
        &self, src_local: BrandedStackValue<'ctx>, src_ty: &Type,
        dest: BrandedValue<'ctx, *mut Val>,
    ) {
        // TODO: pinion enum helpers: types for fields, discriminants
        println!("{:#?}", dest);
        let dest_val = dest.gep_value(self.ctx);

        let disc = match src_ty.get_layout() {
            Layout::Val => {
                // Special case: can just memcpy directly
                self.copy_branded_val(src_local.as_val().unwrap(), dest);
                return;
            },
            Layout::SoftRef => {
                let dest_ptr = unsafe { dest_val.cast_value(self.ctx) };
                self.copy_branded_val(src_local.as_ref().unwrap(), dest_ptr);
                layout::VAL_DISCRIM_REF
            },

            Layout::HardRef(RefType::String) => {
                let dest_ptr = unsafe { dest_val.cast_value(self.ctx) };
                println!("{:#?}", dest_ptr);
                println!("{:#?}", src_local);
                self.copy_branded_val(src_local.as_string().unwrap(), dest_ptr);
                layout::VAL_DISCRIM_STRING
            },
            Layout::HardRef(rt) => {
                assert!(rt.get_layout().is_val());
                let dest_ptr = unsafe { dest_val.cast_value(self.ctx) };
                self.copy_branded_val(src_local.as_ref().unwrap(), dest_ptr);
                layout::VAL_DISCRIM_REF
            },
            Layout::Scalar(ScalarLayout::Null) => layout::VAL_DISCRIM_NULL,
            Layout::Scalar(ScalarLayout::Float) => {
                // TODO: is this valid?
                let dest_ptr = unsafe { dest_val.cast_value(self.ctx) };
                self.copy_branded_val(src_local.as_float().unwrap(), dest_ptr);
                layout::VAL_DISCRIM_FLOAT
            },
        };

        dest.gep_disc(self.ctx)
            .build_store(self.ctx, BrandedValue::<'ctx, u8>::literal(disc, self.ctx));
    }

    fn conv_local_any_into(&self, local_id: LocalId, into: BrandedValue<'ctx, *mut Val>) {
        let local = self.get_local(local_id);
        let ty = self.get_ty(self.ir_func.locals[local_id].ty);
        self.conv_any_into(local, ty, into);
    }

    fn get_local_any_ro(&self, local_id: LocalId) -> BrandedValue<'ctx, *mut Val> {
        let local = self.get_local(local_id);

        // local already points to a val-layout struct
        if let Some(val) = local.as_val() {
            return val;
        }

        let ty = self.get_ty(self.ir_func.locals[local_id].ty);

        let reified_alloca = BrandedValue::<*mut Val>::build_as_alloca(self.ctx);
        self.conv_local_any_into(local_id, reified_alloca);
        reified_alloca
    }

    fn set_local(&mut self, local_id: LocalId, val: BrandedStackValue<'ctx>) {
        assert!(self.locals[local_id].is_none());

        // TODO: do we need lifetimes on locals?
        /*
        let lts_intrinsic = unsafe {
            self.ctx.module.get_intrinsic("llvm.lifetime.start", &[stackptr.get_type().into()]).unwrap()
        };
        self.build_call(lts_intrinsic, &[self.ctx.llvm_ctx.i64_type().const_int(self.locals[local_id].ty.get_store_size(), false).into(), stackptr.into()]);
        */

        // Sketchy is-this-a-global check
        if val.as_bve().unwrap().as_instruction_value().is_some() {
            val.set_name(&format!("local_{}", local_id.index()));
        }
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

                let mut local_struct = self.ctx.get_enum::<Val>().ty.const_zero();
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

    fn bitcast_ptr<T: PinionData>(&self, ptr: PointerValue<'ctx>) -> PointerValue<'ctx> {
        let dest_ty = self
            .ctx
            .get_llvm_type::<T>()
            .ptr_type(inkwell::AddressSpace::Generic);
        self.ctx
            .builder
            .build_bitcast(ptr, dest_ty, "")
            .into_pointer_value()
    }

    fn copy_val(&self, src: PointerValue<'ctx>, dest: PointerValue<'ctx>) {
        self.prog_emit.copy_val(src, dest);
    }

    fn copy_branded_val<T: PinionPointerType>(
        &self, src: BrandedValue<'ctx, T>, dest: BrandedValue<'ctx, T>,
    ) {
        self.copy_val(src.val.into_pointer_value(), dest.val.into_pointer_value())
    }

    fn build_store<P>(&self, ptr: BrandedValue<'ctx, P>, val: BrandedValue<'ctx, P::Element>)
    where
        P: PinionPointerType,
    {
        self.ctx.builder.build_store(ptr.ptr_val(), val.val);
    }

    fn build_call_internal<F>(
        &self, func: F, args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<CallableValue<'ctx>>,
    {
        self.ctx
            .builder
            .build_call(func, &args, "")
            .try_as_basic_value()
            .left()
    }

    fn build_call<F>(
        &self, func: F, args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<CallableValue<'ctx>>,
    {
        /*let fptr = match func.into() {
            Either::Left(func_val) => func_val.as_global_value().as_pointer_value(),
            Either::Right(pointer_val) => pointer_val,
        };*/

        //self.build_call_statepoint(fptr, args)
        self.build_call_internal(func, &args)
    }

    fn build_call_statepoint(
        &self, block: &Block, live: &Lifetimes, func: PointerValue<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>> {
        // TODO: NEEDS TO USE OP BUNDLES FOR LLVM 12+

        let sp_intrinsic = unsafe {
            self.ctx
                .get_intrinsic("llvm.experimental.gc.statepoint", &[func.get_type().into()])
                .unwrap()
        };

        let mut stack_ptrs: Vec<BasicValueEnum> = Vec::new();
        for live_local in live.local.iter().copied() {
            let local = self.locals[live_local].unwrap();
            if local.is_heapptr() {
                let local_ty_id = self.ir_func.locals[live_local].ty;
                stack_ptrs.push(local.as_bve().unwrap());
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
                stack_ptrs.push(self.var_allocs[live_var].as_bve().unwrap());
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
        sp_args.extend(args);
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
            // annoying that we need to go to BMVE here but not for invoke
            let sp_args_bmve: Vec<BasicMetadataValueEnum> =
                sp_args.into_iter().map(Into::into).collect();

            statepoint_token = self
                .ctx
                .builder
                .build_call(sp_intrinsic, &sp_args_bmve, "")
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
                    .get_intrinsic("llvm.experimental.gc.result", &[return_type])
                    .unwrap()
            };
            self.ctx
                .builder
                .build_call(result_intrinsic, &[statepoint_token.into()], "")
                .try_as_basic_value()
                .left()
        } else {
            None
        }
    }

    fn build_call_intrinsic(
        &mut self, intrinsic: Intrinsic, args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>> {
        let func = self.prog_emit.get_intrinsic(intrinsic);
        self.build_call_internal(func, args)
    }

    // TODO: take the right args, or some kind of BrandedValue<ANY>?
    fn build_call_catching<F>(
        &self, block: &Block, func: F, args: &[BasicValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>>
    where
        F: Into<CallableValue<'ctx>>,
    {
        if let Some(pad) = self.ir_func.scopes[block.scope].landingpad {
            let continuation = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "");
            let res = self
                .ctx
                .builder
                .build_invoke(func, &args, continuation, self.blocks[pad], "")
                .try_as_basic_value()
                .left();
            self.ctx.builder.position_at_end(continuation);
            res
        } else {
            let args_bmve: Vec<_> = args.iter().copied().map(Into::into).collect();
            self.build_call_internal(func, &args_bmve)
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
                    // Val into val: alloc and memcpy
                    (Layout::Val, Layout::Val) => {
                        let copied_alloca = BrandedValue::<*mut Val>::build_as_alloca(&self.ctx);
                        self.copy_branded_val(
                            self.var_allocs[*var].as_val().unwrap(),
                            copied_alloca,
                        );
                        copied_alloca.into()
                    },

                    // Soft ref into soft ref: same
                    (Layout::SoftRef, Layout::SoftRef) => {
                        let copied_alloca =
                            BrandedValue::<*mut RttiRef>::build_as_alloca(&self.ctx);
                        self.copy_branded_val(
                            self.var_allocs[*var].as_ref().unwrap(),
                            copied_alloca,
                        );
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
                        self.var_allocs[*var].copy_to_alloca(self.ctx)
                    },
                };

                /*if var_ty.get_layout().is_val() {
                    assert!(
                        local_ty.get_layout().is_val(),
                        "trying to load val-layout var into non-val local"
                    );
                    // var points to reified val struct, so memcpy
                    let copied_alloca = self.build_gc_alloca(self.ctx.get_enum::<Val>().ty, "");
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
                    self.conv_local_any_into(*local, self.var_allocs[*var].as_val().unwrap());
                } else {
                    // trust that types are compatible
                    // tyinfer wouldn't lie to us
                    assert!(local_ty == var_ty);

                    // hmmmm
                    let val = self.get_local(*local);
                    let dest = self.var_allocs[*var];
                    self.copy_val(
                        val.as_bve().unwrap().into_pointer_value(),
                        dest.as_bve().unwrap().into_pointer_value(),
                    );
                };
            },

            Op::Put(id) => {
                let norm = self.get_local_any_ro(*id);

                let cb = ExMod::rt_val_print()
                    .bind(norm.cast_ptr(self.ctx), self.prog_emit.rt_global_val);
                self.ctx.build_call(cb);
            },

            Op::Binary(id, op, lhs, rhs) => {
                let op_var: BrandedValue<BinaryOp> = unsafe {
                    BrandedValue::<u32>::literal(*op as u32, self.ctx).bitcast_self(self.ctx)
                };
                let lhs_ref = self.get_local_any_ro(*lhs);
                let rhs_ref = self.get_local_any_ro(*rhs);
                let res = BrandedValue::<*mut Val>::build_as_alloca(self.ctx);

                let cb = ExMod::rt_val_binary_op().bind(
                    self.prog_emit.rt_global_val,
                    op_var,
                    lhs_ref.cast_ptr(self.ctx),
                    rhs_ref.cast_ptr(self.ctx),
                    res,
                );
                self.ctx.build_call(cb);

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
                let res_alloca = BrandedValue::<*mut Val>::build_as_alloca(self.ctx);
                self.build_call_statepoint(
                    block,
                    &walker.get_cur_lifetimes(),
                    func,
                    &[
                        argpack_ptr.val,
                        self.prog_emit.rt_global.as_basic_value_enum(),
                        res_alloca.val,
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

                let src = self.get_local_any_ro(*src);

                let cb = ExMod::rt_val_cast_string_val()
                    .bind(src.cast_ptr(self.ctx), self.prog_emit.rt_global_val);
                let res: BrandedValue<Option<NonNull<RtString>>> =
                    self.ctx.build_call(cb).result(self.ctx).cast_ptr(self.ctx);

                let res_alloca = res.alloca_emplace(self.ctx);
                self.set_local(*dst, res_alloca.into());
            },

            Op::AllocDatum(dst, ity_id) => {
                // In future, AllocDatum will take a val for the type ID.
                let ty_id_val = self
                    .ctx
                    .llvm_ctx
                    .i32_type()
                    .const_int(ity_id.raw() as u64, false);
                let vt_ptr = unsafe {
                    let ptr = self.ctx.builder.build_in_bounds_gep(
                        self.prog_emit.vt_global.as_pointer_value(),
                        &[self.ctx.llvm_ctx.i32_type().const_zero(), ty_id_val],
                        "vt_ptr",
                    );
                    BrandedValue::<*mut Entry>::materialize(self.ctx, ptr.into())
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
                        self.ctx.get_func(ExFunc::rt_runtime_alloc_datum),
                        &[
                            self.prog_emit.rt_global.as_basic_value_enum(),
                            self.ctx
                                .llvm_ctx
                                .i32_type()
                                .const_int(ity_id.index() as u64, false)
                                .into(),
                        ],
                    )
                    .unwrap();
                let hh_ptr = unsafe {
                    let datum_ptr = BrandedValue::<*mut Datum>::materialize(self.ctx, datum_ptr);
                    datum_ptr.cast_value::<*mut HeapHeader>(self.ctx)
                };

                // SETNAME ref_ptr
                let ref_ptr = BrandedValue::<*mut RttiRef>::build_as_alloca(self.ctx);

                ref_ptr
                    .gep_field(self.ctx, <RttiRef as PinionStruct>::Fields::vptr())
                    .build_store(self.ctx, vt_ptr.cast_ptr(self.ctx));
                ref_ptr
                    .gep_field(self.ctx, <RttiRef as PinionStruct>::Fields::ptr())
                    .build_store(self.ctx, hh_ptr.cast_ptr(self.ctx));

                // TODO: embed datum ty info
                self.set_local(*dst, ref_ptr.into());
            },

            Op::DatumLoadVar(dst, src, var_id) => {
                assert!(self
                    .get_ty(self.ir_func.locals[*dst].ty)
                    .get_layout()
                    .is_val());

                let rval_ptr = self.soft_cast_val_ref(*src);
                let (ref_vptr, ref_ptr) = self.build_extract_ref_ptrs(rval_ptr);
                let var_get_ptr = ref_vptr
                    .gep_field(self.ctx, <Entry as PinionStruct>::Fields::var_get())
                    .build_load(self.ctx);

                // TODO: drop/cloned
                let datum_ptr = unsafe { ref_ptr.cast_value::<*mut Datum>(self.ctx) };

                let loaded_alloca = BrandedValue::<*mut Val>::build_as_alloca(self.ctx);
                self.build_call_catching::<CallableValue>(
                    block,
                    var_get_ptr.val.into_pointer_value().try_into().unwrap(),
                    &[
                        datum_ptr.val,
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(var_id.index() as u64, false)
                            .into(),
                        loaded_alloca.val,
                    ],
                );
                self.set_local(*dst, loaded_alloca.into());
            },

            Op::DatumStoreVar(dst, var_id, src) => {
                println!(
                    "whopr {:?} {:?} {:?}",
                    dst,
                    self.get_ty(self.ir_func.locals[*dst].ty),
                    self.get_local(*dst)
                );
                let lval_ptr = self.soft_cast_val_ref(*dst);
                let (ref_vptr, ref_ptr) = self.build_extract_ref_ptrs(lval_ptr);
                let var_set_ptr = ref_vptr
                    .gep_field(self.ctx, <Entry as PinionStruct>::Fields::var_set())
                    .build_load(self.ctx);

                // TODO: hmmmm
                let datum_ptr = unsafe { ref_ptr.cast_value::<*mut Datum>(self.ctx) };

                let src_val = self.get_local_any_ro(*src);
                // TODO: blech, probably change generics
                self.build_call_catching::<CallableValue>(
                    block,
                    var_set_ptr.val.into_pointer_value().try_into().unwrap(),
                    &[
                        datum_ptr.val,
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(var_id.index() as u64, false)
                            .into(),
                        src_val.val,
                    ],
                );
            },

            Op::DatumCallProc(_dst, _src, _proc_name, _args) => {
                todo!("very broken");
                /*let argpack_ptr = self.build_argpack(Some(*src), args);
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

                        let res_alloca = self.build_gc_alloca(self.ctx.get_enum::<Val>().ty, "");
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
                        let res_alloca = self.build_gc_alloca(self.ctx.get_enum::<Val>().ty, "");
                        self.build_call_statepoint(
                            block,
                            &walker.get_cur_lifetimes(),
                            self.ctx
                                .get_func(ExFunc::rt_val_call_proc)
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
                }*/
            },

            Op::Throw(exception_local) => {
                let exception_val = self.get_local_any_ro(*exception_local);
                self.build_call_catching(block, self.ctx.rt.rt_throw, &[exception_val.val]);
            },

            Op::CatchException(maybe_except_var) => {
                if idx != 0 {
                    panic!("CatchException op not at start of block");
                }

                let landingpad = self.ctx.builder.build_landing_pad(
                    self.ctx.rt.ty.landingpad_type,
                    self.ctx.rt.dm_eh_personality,
                    &[],
                    true,
                    "lp",
                );

                if let Some(except_var) = maybe_except_var {
                    let exception_container = self
                        .ctx
                        .builder
                        .build_extract_value(landingpad.into_struct_value(), 0, "")
                        .unwrap();
                    assert!(self
                        .get_ty(self.ir_func.vars[*except_var].ty)
                        .get_layout()
                        .is_val());
                    self.build_call(
                        self.ctx.rt.rt_exception_get_val,
                        &[
                            exception_container.into(),
                            self.var_allocs[*except_var].as_bve().unwrap().into(),
                        ],
                    );
                }
            },

            Op::Spawn(closure_slot, delay) => {
                todo!();
                /*assert!(delay.is_none());
                println!("in spawn");

                let func_id = self.ir_func.child_closures[*closure_slot];
                let cv = self.gather_captured_vars(func_id);

                println!("cv: {:?}", cv);

                // For now, all closure vars are soft/Any.
                // TODO: support types on closure boundary

                let cap_array = self.ctx.builder.build_array_alloca(
                    self.ctx.get_enum::<Val>().ty,
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
                        .get_func(ExFunc::rt_runtime_spawn_closure)
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
                            .build_address_space_cast(
                                cap_array,
                                self.ctx
                                    .get_enum::<Val>()
                                    .ty
                                    .ptr_type(inkwell::AddressSpace::Generic),
                                "",
                            )
                            .into(),
                    ],
                );*/
            },

            Op::Sleep(delay) => {
                let local = self.get_local(*delay);
                println!("{:?}", walker.get_cur_lifetimes());
                self.build_call(
                    self.ctx.get_func(ExFunc::rt_runtime_suspend),
                    &[
                        self.prog_emit.rt_global.as_pointer_value().into(),
                        local.as_val().unwrap().val.into(),
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
                let ret_out = unsafe {
                    BrandedValue::<*mut Val>::materialize(self.ctx, self.ll_func.get_params()[2])
                };
                let ret_ty = self.get_ty(self.ir_func.vars[0].ty);
                if ret_ty.get_layout().is_val() {
                    self.copy_branded_val(self.var_allocs[0].as_val().unwrap(), ret_out);
                } else {
                    self.conv_any_into(self.var_allocs[0], ret_ty, ret_out);
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
                    Type::Null => BrandedValue::<i32>::literal(0, self.ctx),
                    Type::Float => {
                        let disc_val = self
                            .get_local(*discriminant)
                            .as_float()
                            .unwrap()
                            .build_load(self.ctx);
                        disc_val.cast_f32_to_i32(self.ctx)
                    },
                    Type::Any | Type::OneOf(_) => {
                        let disc_val = self.get_local(*discriminant).as_val().unwrap();
                        let ret = self
                            .build_call_catching(
                                block,
                                self.ctx.get_func(ExFunc::rt_val_to_switch_disc),
                                &[disc_val.val],
                            )
                            .unwrap();
                        unsafe { BrandedValue::<i32>::materialize(self.ctx, ret) }
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

                self.ctx.builder.build_switch(
                    disc_int.val.into_int_value(),
                    self.blocks[*default],
                    &llvm_branches[..],
                );
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
        &mut self, block: &Block, op: op::HardBinary, lhs: BrandedStackValue<'ctx>,
        rhs: BrandedStackValue<'ctx>,
    ) -> BrandedStackValue<'ctx> {
        if let op::HardBinary::StringConcat = op {
            let res = self
                .build_call_catching(
                    block,
                    self.ctx.get_func(ExFunc::rt_runtime_concat_strings),
                    &[
                        self.prog_emit.rt_global.as_basic_value_enum(),
                        lhs.as_string().unwrap().build_load(self.ctx).val,
                        rhs.as_string().unwrap().build_load(self.ctx).val,
                    ],
                )
                .unwrap();
            return unsafe {
                BrandedValue::<Option<NonNull<RtString>>>::materialize(self.ctx, res)
            }
            .alloca_emplace(self.ctx)
            .into();
        }

        let lhs_bve = lhs.as_float().unwrap().build_load(self.ctx).val;
        let rhs_bve = rhs.as_float().unwrap().build_load(self.ctx).val;
        match op {
            op::HardBinary::StringConcat => unreachable!(),
            op::HardBinary::FloatAdd => {
                let res = self
                    .ctx
                    .builder
                    .build_float_add(lhs_bve.into_float_value(), rhs_bve.into_float_value(), "")
                    .into();
                // ewwww TODO: cleanup cleanup
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatSub => {
                let res = self
                    .ctx
                    .builder
                    .build_float_sub(lhs_bve.into_float_value(), rhs_bve.into_float_value(), "")
                    .into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatMul => {
                let res = self
                    .ctx
                    .builder
                    .build_float_mul(lhs_bve.into_float_value(), rhs_bve.into_float_value(), "")
                    .into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatDiv => {
                let res = self
                    .ctx
                    .builder
                    .build_float_div(lhs_bve.into_float_value(), rhs_bve.into_float_value(), "")
                    .into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatMod => {
                let res = self
                    .ctx
                    .builder
                    .build_float_rem(lhs_bve.into_float_value(), rhs_bve.into_float_value(), "")
                    .into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatPow => {
                let res = self
                    .build_call_intrinsic(Intrinsic::FPow, &[lhs_bve.into(), rhs_bve.into()])
                    .unwrap();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatCmp(pred) => {
                let bool_res = self.ctx.builder.build_float_compare(
                    Self::conv_pred(pred),
                    lhs_bve.into_float_value(),
                    rhs_bve.into_float_value(),
                    "",
                );
                let res = self
                    .ctx
                    .builder
                    .build_unsigned_int_to_float(bool_res, self.ctx.llvm_ctx.f32_type(), "")
                    .into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
            op::HardBinary::FloatBitOp(bitop) => {
                let lhs_i24 = self.float_to_i24(lhs_bve.into_float_value());
                let rhs_i24 = self.float_to_i24(rhs_bve.into_float_value());
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
                let res = self.i24_to_float(res_i24).into();
                let alloca = BrandedValue::<*mut f32>::build_as_alloca(self.ctx);
                alloca.build_store(self.ctx, unsafe {
                    BrandedValue::<f32>::materialize(self.ctx, res)
                });
                alloca.into()
            },
        }
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

    fn build_argpack(
        &self, src: Option<LocalId>, args: &[LocalId],
    ) -> BrandedValue<'ctx, *mut ProcPack> {
        let n_val = self
            .ctx
            .llvm_ctx
            .i64_type()
            .const_int(args.len() as u64, false);
        // SETNAME unnamed_args
        let argpack_unnamed =
            BrandedValue::<'ctx, *mut Val>::build_as_alloca_array(self.ctx, args.len() as _);
        for (i, arg) in args.iter().enumerate() {
            let ptr = unsafe { argpack_unnamed.array_gep(self.ctx, i as _) };
            self.conv_local_any_into(*arg, ptr);
        }

        // SETNAME argpack
        let argpack = BrandedValue::<*mut ProcPack>::build_as_alloca(self.ctx);
        unsafe {
            let unnamed_field =
                argpack.gep_field(self.ctx, <ProcPack as PinionStruct>::Fields::unnamed());

            unnamed_field
                .gep_field(self.ctx, <FfiArray<Val> as PinionStruct>::Fields::len())
                .build_store(
                    self.ctx,
                    BrandedValue::<u64>::literal(args.len() as _, self.ctx),
                );
            unnamed_field
                .gep_field(self.ctx, <FfiArray<Val> as PinionStruct>::Fields::data())
                .build_store(self.ctx, argpack_unnamed.cast_ptr(self.ctx));

            let named_field =
                argpack.gep_field(self.ctx, <ProcPack as PinionStruct>::Fields::named());

            named_field
                .gep_field(
                    self.ctx,
                    <FfiArray<(StringId, Val)> as PinionStruct>::Fields::len(),
                )
                .build_store(self.ctx, BrandedValue::<u64>::literal(0, self.ctx));

            if let Some(id) = src {
                let src_field =
                    argpack.gep_field(self.ctx, <ProcPack as PinionStruct>::Fields::src());
                self.conv_local_any_into(id, src_field);
            }
        }

        argpack
    }

    fn soft_cast_val_ref(&self, local: LocalId) -> BrandedValue<'ctx, *mut RttiRef> {
        match self.get_local(local) {
            BrandedStackValue::Val(val_val) => {
                // TODO: pinion enum helpers
                let disc = val_val.gep_disc(self.ctx).build_load(self.ctx);

                let trap_block = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "");
                let continuation = self.ctx.llvm_ctx.append_basic_block(self.ll_func, "");

                let ref_disc = BrandedValue::<u8>::literal(layout::VAL_DISCRIM_REF, self.ctx);

                self.ctx.builder.build_switch(
                    disc.val.into_int_value(),
                    trap_block,
                    &[(ref_disc.val.into_int_value(), continuation)],
                );

                self.ctx.builder.position_at_end(trap_block);
                let trap_fn = self.prog_emit.get_intrinsic(Intrinsic::Trap);
                self.ctx.builder.build_call(trap_fn, &[], "");
                self.ctx.builder.build_unreachable();

                self.ctx.builder.position_at_end(continuation);
                unsafe { val_val.gep_value(self.ctx).cast_value(self.ctx) }
            },
            BrandedStackValue::Ref(ref_val) => ref_val,
            BrandedStackValue::Null
            | BrandedStackValue::Float(_)
            | BrandedStackValue::String(_) => panic!("can't extract"),
        }
    }

    /// Returns (vptr, ptr).
    fn build_extract_ref_ptrs(
        &self, rval_ptr: BrandedValue<'ctx, *mut RttiRef>,
    ) -> (
        BrandedValue<'ctx, *mut Entry>,
        BrandedValue<'ctx, *mut HeapHeader>,
    ) {
        // TODO: tycheck we're a ref
        // TODO: use some constant instead of 1
        // TODO: Pinion struct helpers
        // TODO: cast val struct instead of int2ptr
        println!("{:?}", rval_ptr);
        let val_vptr = rval_ptr
            .gep_field(self.ctx, <RttiRef as PinionStruct>::Fields::vptr())
            .build_load(self.ctx);
        let val_ptr = rval_ptr
            .gep_field(self.ctx, <RttiRef as PinionStruct>::Fields::ptr())
            .build_load(self.ctx);
        /*let ref_vptr_int = self
            .ctx
            .builder
            .build_load(val_vptr, "ref_vptr_int")
            .into_int_value();
        let ref_ptr_int = self
            .ctx
            .builder
            .build_load(val_ptr, "ref_ptr_int")
            .into_int_value();*/

        (
            val_vptr.cast_ptr(self.ctx),
            val_ptr.cast_ptr(self.ctx), /*self.ctx.builder.build_int_to_ptr(
                                            ref_vptr_int,
                                            self.ctx.get_type_ptr::<vtable::Entry>(),
                                            "ref_vptr",
                                        ),
                                        self.ctx.builder.build_int_to_ptr(
                                            ref_ptr_int,
                                            self.ctx.get_type_ptr::<Datum>(),
                                            "ref_ptr",
                                        ),*/
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
        unsafe { self.const_gep_extractvalue(vptr, &[0, offset]) }
        /*self.ctx.builder.build_in_bounds_gep(
            vptr,
            &[
                self.ctx.llvm_ctx.i32_type().const_zero(),
                self.ctx.llvm_ctx.i32_type().const_int(offset, false),
            ],
            &format!("vtable_field_{}_ptr", offset),
        )*/

        /*self.ctx
        .builder
        .build_load(field_ptr, &format!("vtable_field_{}", offset))*/
    }

    fn build_var_zeroinit(&self, var: VarId) {
        let var_ty = self.get_ty(self.ir_func.vars[var].ty);
        let val = self.var_allocs[var];

        match val {
            BrandedStackValue::Val(val) => {
                let disc_ptr = val.gep_disc(self.ctx);
                let null_disc = BrandedValue::literal(layout::VAL_DISCRIM_NULL, self.ctx);
                self.build_store(disc_ptr, null_disc);
            },
            BrandedStackValue::Null => {},
            BrandedStackValue::Float(val) => {
                let zero_float = BrandedValue::literal(0f32, self.ctx);
                self.build_store(val, zero_float);
            },
            BrandedStackValue::String(_) => todo!(),
            BrandedStackValue::Ref(_) => todo!(),
        }

        /*match var_ty.get_layout() {
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
        }*/
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
                //let local = &self.ir_func.locals[*local_id];
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
