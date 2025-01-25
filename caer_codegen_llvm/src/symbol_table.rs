use std::collections::HashMap;
use std::ptr::NonNull;

use caer_ir::cfg::Function;
use caer_ir::module::Module;
use caer_runtime::arg_pack::ProcPack;
use caer_runtime::heap_object::{GcMarker, HeapHeader};
use caer_runtime::runtime::Runtime;
use caer_runtime::string::RtString;
use caer_runtime::val::Val;
use caer_runtime::vtable::{self, FuncPtr};
use caer_types::id::{FuncId, StringId};
use index_vec::IndexVec;
use inkwell::types::{BasicType, FunctionType};
use inkwell::values::{FunctionValue, GlobalValue};

use crate::context::{Context, ExFunc, ExRuntime};
use crate::type_manager::TypeManager;
use crate::value::BrandedValue;

#[derive(Debug)]
pub struct SymbolTable<'ctx> {
    ir_env: &'ctx Module,
    ir_funcs: IndexVec<FuncId, &'ctx Function>,
    ir_llvm_funcs: IndexVec<FuncId, FunctionValue<'ctx>>,
    external_llvm_funcs: HashMap<ExFunc, FunctionValue<'ctx>>,

    // TODO: this really shouldn't be here...
    pub proc_type: FunctionType<'ctx>,

    // TODO: maybe move these out too, they're not exactly "symbols"
    pub rt_global: GlobalValue<'ctx>,
    pub rt_global_val: BrandedValue<'ctx, *mut Runtime>,
    pub vt_global: GlobalValue<'ctx>,
    pub ft_global: GlobalValue<'ctx>,

    pub string_table: IndexVec<StringId, BrandedValue<'ctx, Option<NonNull<RtString>>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new(ctx: &Context<'ctx>, tys: &TypeManager<'ctx>, ir_env: &'ctx Module) -> Self {
        let ir_funcs = ir_env.funcs.iter().collect();

        let proc_type = {
            let argpack_ptr_type = tys.get_llvm_type::<*const ProcPack>();
            let val_ptr_type = tys.get_llvm_type::<*mut Val>();
            let rt_ptr_type = tys.get_llvm_type::<*mut Runtime>();
            ctx.llvm_ctx.void_type().fn_type(
                &[
                    argpack_ptr_type.into(),
                    rt_ptr_type.into(),
                    val_ptr_type.into(),
                ],
                false,
            )
        };

        let rt_type = tys.get_llvm_type::<Runtime>();
        let funcptr_ty = tys.get_llvm_type::<FuncPtr>();

        let rt_global = ctx.module.add_global(rt_type, None, "runtime");
        rt_global.set_initializer(&rt_type.const_zero());

        let rt_global_val = unsafe {
            BrandedValue::<*mut Runtime>::materialize(&ctx, rt_global.as_pointer_value().into())
        };

        // TODO: don't dig so deep into env?
        let vt_global_ty = tys
            .get_llvm_type::<vtable::Entry>()
            .array_type(ir_env.type_tree.len() as u32);
        let vt_global = ctx.module.add_global(vt_global_ty, None, "vtable");
        vt_global.set_constant(true);

        let ft_global_ty = funcptr_ty.array_type(ir_env.funcs.len() as u32);
        let ft_global = ctx.module.add_global(ft_global_ty, None, "ftable");
        ft_global.set_constant(true);

        let string_table = Self::initialize_string_table(ctx, tys, ir_env);

        Self {
            ir_env,
            ir_funcs,
            ir_llvm_funcs: Self::initialize_ir_funcs(ctx, tys, ir_env, proc_type),
            external_llvm_funcs: Self::initialize_external_funcs(ctx, tys),
            proc_type,
            rt_global,
            rt_global_val,
            vt_global,
            ft_global,
            string_table,
        }
    }

    // These could probably live somewhere else
    fn initialize_ir_funcs(
        ctx: &Context<'ctx>, tys: &TypeManager<'ctx>, ir_env: &'ctx Module,
        proc_type: FunctionType<'ctx>,
    ) -> IndexVec<FuncId, FunctionValue<'ctx>> {
        ir_env
            .funcs
            .iter()
            .map(|func| {
                let ty = if func.closure.is_some() {
                    tys.rt.ty.closure_type
                } else {
                    proc_type
                };

                let ll_func =
                    ctx.module
                        .add_function(&format!("proc_{}", func.id.index()), ty, None);
                ll_func.set_personality_function(tys.rt.dm_eh_personality);
                ll_func.set_gc("statepoint-example");

                ll_func
            })
            .collect()
    }

    fn initialize_external_funcs(
        ctx: &Context<'ctx>, tys: &TypeManager<'ctx>,
    ) -> HashMap<ExFunc, FunctionValue<'ctx>> {
        tys.get_external_funcs()
            .map(|(func_enum, (layout, ty))| {
                let val = ctx.module.add_function(layout.name, ty, None);
                (func_enum, val)
            })
            .collect()
    }

    fn initialize_string_table(
        ctx: &Context<'ctx>, tys: &TypeManager<'ctx>, ir_env: &'ctx Module,
    ) -> IndexVec<StringId, BrandedValue<'ctx, Option<NonNull<RtString>>>> {
        let string_repr = tys.get_struct::<RtString>();
        let hh_repr = tys.get_struct::<HeapHeader>();
        let gcm_repr = tys.get_enum::<GcMarker>();

        let string_globals: IndexVec<StringId, _> = ir_env
            .string_table
            .iter()
            .map(|(id, s)| {
                let cs = ctx.llvm_ctx.const_string(s.as_bytes(), false);
                let g = ctx
                    .module
                    .add_global(cs.get_type(), None, &format!("string_{}", id.raw()));
                g.set_initializer(&cs);
                g.set_constant(true);
                (g, s.len())
            })
            .collect();

        // This is split into two passes to make the resulting IR look a bit nicer
        // TODO: build with nicer pinion types
        string_globals
            .into_iter_enumerated()
            .map(|(id, (string_global, len))| {
                let alloc_string = string_repr.ty.const_named_struct(&[
                    hh_repr
                        .ty
                        .const_named_struct(&[gcm_repr
                            .ty
                            .const_int(GcMarker::White as _, false)
                            .into()])
                        .into(),
                    ctx.llvm_ctx.i64_type().const_int(len as _, false).into(),
                    unsafe {
                        ctx.builder
                            .build_in_bounds_gep(
                                string_global.as_pointer_value(),
                                &[
                                    ctx.llvm_ctx.i32_type().const_zero(),
                                    ctx.llvm_ctx.i32_type().const_zero(),
                                ],
                                "",
                            )
                            .unwrap()
                    }
                    .into(),
                ]);
                let asg = ctx.module.add_global(
                    string_repr.ty,
                    None,
                    &format!("alloc_string_{}", id.raw()),
                );
                asg.set_initializer(&alloc_string);
                asg.set_constant(true);
                unsafe { BrandedValue::materialize(&ctx, asg.as_pointer_value().into()) }
            })
            .collect()
    }

    pub fn get_func(&self, func: ExFunc) -> FunctionValue<'ctx> {
        *self.external_llvm_funcs.get(&func).unwrap()
    }

    pub fn lookup_global_proc(&self, name: StringId) -> FuncId {
        let global_dty = self.ir_env.type_tree.global_type();
        global_dty.proc_lookup[&name].top_func
    }

    pub fn get_ir_llvm_func(&self, id: FuncId) -> Option<FunctionValue<'ctx>> {
        self.ir_llvm_funcs.get(id).copied()
    }

    pub fn iter_ir_llvm_funcs(&'ctx self) -> impl Iterator<Item = (FuncId, FunctionValue<'ctx>)> {
        self.ir_llvm_funcs.iter_enumerated().map(|(i, f)| (i, *f))
    }
}
