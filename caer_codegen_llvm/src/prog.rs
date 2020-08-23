use super::proc::ProcEmit;
use super::context::Context;
use caer_ir::cfg::*;
use caer_ir::env::Env;
use caer_types::rt_env::RtEnv;
use caer_types::layout;
use caer_types::type_tree::{DType, Specialization};
use caer_types::id::{StringId, TypeId, ProcId};
use index_vec::IndexVec;
use std::fs::{self, File};
use inkwell::values::AnyValueEnum;
use crate::context::GC_ADDRESS_SPACE;
use caer_ir::walker::CFGWalker;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ProgEmit<'a, 'ctx> {
    pub ctx: &'a Context<'a, 'ctx>,
    pub env: &'a Env,
    pub procs: Vec<(&'a Proc, inkwell::values::FunctionValue<'ctx>)>,
    pub rt_global: inkwell::values::GlobalValue<'ctx>,
    pub vt_global: inkwell::values::GlobalValue<'ctx>,
    pub vt_lookup: Vec<inkwell::values::FunctionValue<'ctx>>,
    pub datum_types: IndexVec<TypeId, inkwell::types::StructType<'ctx>>,
    pub sym: IndexVec<ProcId, inkwell::values::FunctionValue<'ctx>>,
}

impl<'a, 'ctx> ProgEmit<'a, 'ctx> {
    pub fn new(ctx: &'a Context<'a, 'ctx>, env: &'a Env) -> Self {
        let rt_global = ctx.module.add_global(ctx.rt.ty.rt_type, Some(inkwell::AddressSpace::Generic), "runtime");
        rt_global.set_initializer(&ctx.rt.ty.rt_type.const_zero());

        // TODO: don't dig so deep into env?
        let vt_global_ty = ctx.rt.ty.vt_entry_type.array_type(env.type_tree.types.len() as u32);
        let vt_global = ctx.module.add_global(vt_global_ty, Some(inkwell::AddressSpace::Generic), "vtable");
        vt_global.set_constant(true);

        Self {
            ctx,
            env,
            procs: Vec::new(),
            rt_global,
            vt_global,
            vt_lookup: ctx.make_vtable_lookup(vt_global),
            datum_types: IndexVec::new(),
            sym: IndexVec::new(),
        }
    }

    pub fn build_procs(&mut self) {
        for proc in self.env.procs.iter() {
            self.add_proc(proc);
        }
    }

    pub fn lookup_global_proc(&self, name: StringId) -> inkwell::values::FunctionValue<'ctx> {
        let global_dty = self.env.type_tree.global_type();
        let proc_id = global_dty.proc_lookup[&name].top_proc;
        self.sym[proc_id]
    }

    fn add_proc(&mut self, proc: &'a Proc) {
        //let mut proc_emit = ProcEmit::new(self.ctx, proc, name);
        let func = self.ctx.module.add_function(&format!("proc_{}", proc.id.index()), self.ctx.rt.ty.proc_type, None);
        func.set_personality_function(self.ctx.rt.dm_eh_personality);
        func.set_gc("statepoint-example");

        assert_eq!(proc.id.index(), self.sym.len());
        self.sym.push(func);
        self.procs.push((proc, func));
    }

    pub fn emit(&mut self) {
        self.populate_datum_types();
        self.emit_vtable();
        let main_block = self.emit_main();

        for (proc, func) in self.procs.drain(..).collect::<Vec<_>>() { // TODO no
            println!("EMITTING {:?}", proc.id);
            let walker = CFGWalker::build(proc);
            let mut proc_emit = ProcEmit::new(self.ctx, self, proc, func);
            walker.walk(proc, &mut proc_emit);
        }
        let main_proc = self.lookup_global_proc(self.env.intern_string_ro("entry"));

        self.finalize_main(main_block, main_proc);
    }

    fn emit_main(&mut self) -> inkwell::basic_block::BasicBlock<'ctx> {
        let func_type = self.ctx.llvm_ctx.void_type().fn_type(&[], false);
        let func = self.ctx.module.add_function("main", func_type, None);

        let block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(block);

        // TODO: move file emit to a better place
        let rt_env = RtEnv {
            type_tree: self.env.type_tree.clone(),
            proc_specs: self.env.proc_specs.clone(),
        };
        self.env.string_table.serialize_runtime(File::create("stringtable.bincode").unwrap());
        bincode::serialize_into(File::create("environment.bincode").unwrap(), &rt_env).unwrap();

        serde_json::to_writer_pretty(File::create("environment.json").unwrap(), &rt_env).unwrap();

        let vt_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(self.vt_global.as_pointer_value(), &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            self.ctx.llvm_ctx.i32_type().const_zero(),
        ], "vt_ptr") };

        // defined in linker script
        let stackmap_start = self.ctx.module.add_global(self.ctx.llvm_ctx.i8_type(), None, "__stackmaps_start");
        stackmap_start.set_linkage(inkwell::module::Linkage::External);
        let stackmap_end = self.ctx.module.add_global(self.ctx.llvm_ctx.i8_type(), None, "__stackmaps_end");
        stackmap_end.set_linkage(inkwell::module::Linkage::External);

        self.ctx.builder.build_call(self.ctx.rt.rt_runtime_init, &[
            self.rt_global.as_pointer_value().into(),
            stackmap_start.as_pointer_value().into(),
            stackmap_end.as_pointer_value().into(),
            vt_ptr.into(),
        ], "");

        block
    }

    fn finalize_main(&self, block: inkwell::basic_block::BasicBlock, entry_func: inkwell::values::FunctionValue) {
        self.ctx.builder.position_at_end(block);
        let argpack_alloca = self.ctx.builder.build_alloca(self.ctx.rt.ty.arg_pack_type, "argpack_ptr");
        let argpack = self.ctx.rt.ty.arg_pack_type.const_zero();
        self.ctx.builder.build_store(argpack_alloca, argpack);
        let ret_val = self.ctx.builder.build_alloca(self.ctx.rt.ty.val_type, "ret_val");
        let ret_val_gc = self.ctx.builder.build_address_space_cast(ret_val, self.ctx.rt.ty.val_type_ptr, "");
        self.ctx.builder.build_call(entry_func, &[argpack_alloca.into(), self.rt_global.as_pointer_value().into(), ret_val_gc.into()], "");
        self.ctx.builder.build_return(None);
    }

    fn populate_datum_types(&mut self) {
        for ty in self.env.type_tree.types.iter() {
            let vars_field_ty = self.ctx.rt.ty.val_type.array_type(ty.vars.len() as u32);
            let datum_ty = self.ctx.llvm_ctx.struct_type(&[
                self.ctx.llvm_ctx.i32_type().into(),
                vars_field_ty.into(),
            ], false);
            assert_eq!(ty.id.index(), self.datum_types.len());
            self.datum_types.push(datum_ty);
        }
    }

    fn emit_vtable(&mut self) {
        let mut vt_entries = Vec::new();
        // yuck, TODO: encapsulate typetree
        for ty in self.env.type_tree.types.iter() {
            let size_val = self.datum_types[ty.id].size_of().unwrap().const_cast(self.ctx.llvm_ctx.i64_type(), false);

            let entries = &match ty.specialization {
                Specialization::Datum => {
                    let var_index_fn = self.make_var_index_func(ty);
                    [
                        size_val.into(),
                        self.make_get_var_func(ty, var_index_fn).into(),
                        self.make_set_var_func(ty, var_index_fn).into(),
                        self.make_proc_lookup_func(ty).into(),
                    ]
                }
                Specialization::List => {
                    [
                        size_val.into(),
                        self.ctx.rt.rt_list_var_get.into(),
                        self.ctx.rt.rt_list_var_set.into(),
                        self.ctx.rt.rt_list_proc_lookup.into(),
                    ]
                }
            };

            let field_tys = self.ctx.rt.ty.vt_entry_type.get_field_types();
            assert_eq!(entries.len(), field_tys.len());
            let cast_entries: Vec<_> = entries.into_iter().enumerate().map(|(i, entry)| {
                match *entry {
                    AnyValueEnum::FunctionValue(f) => self.ctx.builder.build_bitcast(f.as_global_value().as_pointer_value(), field_tys[i], "").into(),
                    AnyValueEnum::IntValue(v) => v.into(),
                    _ => unimplemented!("add handler"),
                }

            }).collect();

            let vt_entry = self.ctx.rt.ty.vt_entry_type.const_named_struct(&cast_entries);
            vt_entries.push(vt_entry);
        }
        self.vt_global.set_initializer(&self.ctx.rt.ty.vt_entry_type.const_array(&vt_entries));
    }

    fn make_var_index_func(&mut self, ty: &DType) -> inkwell::values::FunctionValue<'ctx> {
        let func_ty = self.ctx.llvm_ctx.i32_type().fn_type(&[self.ctx.llvm_ctx.i64_type().into()], false);
        // TODO: MANGLE
        let func = self.ctx.module.add_function(&format!("ty_{}_var_index", ty.id.index()), func_ty, None);
        // TODO: revisit
        let attr = self.ctx.llvm_ctx.create_string_attribute("alwaysinline", "");
        func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        let dropout_block = self.ctx.llvm_ctx.append_basic_block(func, "dropout");
        let conv_block = self.ctx.llvm_ctx.append_basic_block(func, "conv");

        self.ctx.builder.position_at_end(dropout_block);

        /*
        let exception_path = self.env.intern_string_ro("/exception");
        let exception_ty = self.env.type_tree.type_by_path_str[&exception_path];
        let datum_ptr = self.ctx.builder.build_call(self.ctx.rt.rt_runtime_alloc_datum, &[
            self.rt_global.as_pointer_value().into(),
            self.ctx.llvm_ctx.i32_type().const_int(exception_ty.index() as _, false).into(),
        ], "").try_as_basic_value().left().unwrap();
        self.ctx.builder.build_call(self.ctx.rt.rt_throw, &[datum_ptr.into()], "");
        */
        // TODO: RTE no such var
        let trap_fn = self.get_intrinsic(Intrinsic::Trap);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        let mut cases = Vec::new();
        let mut phi_incoming = Vec::new();
        for (i, var_name) in ty.vars.iter().enumerate() {
            let case_block = self.ctx.llvm_ctx.append_basic_block(func, &format!("case_{}", var_name.index()));
            self.ctx.builder.position_at_end(case_block);
            let disc_val = self.ctx.llvm_ctx.i64_type().const_int(var_name.index() as u64, false);
            let this_offset_val = self.ctx.llvm_ctx.i32_type().const_int(i as u64, false);
            self.ctx.builder.build_unconditional_branch(conv_block);
            cases.push((disc_val, case_block));
            phi_incoming.push((this_offset_val, case_block));
        }

        self.ctx.builder.position_at_end(entry_block);

        if cases.len() == 0 {
            // hack, TODO: replace
            self.ctx.builder.build_unconditional_branch(conv_block);
            self.ctx.builder.position_at_end(conv_block);
            self.ctx.builder.build_unconditional_branch(dropout_block);
            return func
        }

        let param_val = func.get_first_param().unwrap().into_int_value();
        self.ctx.builder.build_switch(param_val, dropout_block, &cases);

        self.ctx.builder.position_at_end(conv_block);
        let phi_incoming: Vec<_> = phi_incoming.iter().map(|(v, b)| (v as _, *b)).collect();
        let offset_val = self.ctx.builder.build_phi(self.ctx.llvm_ctx.i32_type(), "offset");
        offset_val.add_incoming(phi_incoming.as_slice());
        self.ctx.builder.build_return(Some(&offset_val.as_basic_value()));

        func
    }

    fn make_get_var_func(&self, ty: &DType, index_func: inkwell::values::FunctionValue<'ctx>) -> inkwell::values::FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.rt.ty.val_type.fn_type(&[datum_type_ptr.into(), self.ctx.llvm_ctx.i64_type().into()], false);
        // TODO: MANGLE
        let func = self.ctx.module.add_function(&format!("ty_{}_var_get", ty.id.index()), func_ty, None);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(entry_block);

        let datum_ptr = func.get_first_param().unwrap().into_pointer_value();
        let var_name = func.get_last_param().unwrap().into_int_value();

        let var_index = self.ctx.builder.build_call(index_func, &[var_name.into()], "var_index").try_as_basic_value().left().unwrap().into_int_value();
        let var_val_ptr = unsafe {self.ctx.builder.build_in_bounds_gep(datum_ptr, &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            self.ctx.llvm_ctx.i32_type().const_int(layout::DATUM_VARS_FIELD_OFFSET, false),
            var_index,
        ], "var_val_ptr")};
        let var_val = self.ctx.builder.build_load(var_val_ptr, "var_val");
        self.ctx.builder.build_return(Some(&var_val));

        func
    }

    fn make_set_var_func(&self, ty: &DType, index_func: inkwell::values::FunctionValue<'ctx>) -> inkwell::values::FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.llvm_ctx.void_type().fn_type(&[datum_type_ptr.into(), self.ctx.llvm_ctx.i64_type().into(), self.ctx.rt.ty.val_type.into()], false);
        // TODO: MANGLE
        let func = self.ctx.module.add_function(&format!("ty_{}_var_set", ty.id.index()), func_ty, None);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(entry_block);

        let datum_ptr = func.get_params()[0].into_pointer_value();
        let var_name = func.get_params()[1].into_int_value();
        let asg_val = func.get_params()[2].into_struct_value();

        let var_index = self.ctx.builder.build_call(index_func, &[var_name.into()], "var_index").try_as_basic_value().left().unwrap().into_int_value();
        let var_val_ptr = unsafe {self.ctx.builder.build_in_bounds_gep(datum_ptr, &[
            self.ctx.llvm_ctx.i32_type().const_zero(),
            self.ctx.llvm_ctx.i32_type().const_int(layout::DATUM_VARS_FIELD_OFFSET, false),
            var_index,
        ], "var_val_ptr")};
        self.ctx.builder.build_store(var_val_ptr, asg_val);
        self.ctx.builder.build_return(None);

        func
    }

    fn make_proc_lookup_func(&mut self, ty: &DType) -> inkwell::values::FunctionValue<'ctx> {
        let ret_ty = self.ctx.rt.ty.proc_type.ptr_type(inkwell::AddressSpace::Generic);
        let func_ty = ret_ty.fn_type(&[self.ctx.llvm_ctx.i64_type().into()], false);
        let func = self.ctx.module.add_function(&format!("ty_{}_proc_lookup", ty.id.index()), func_ty, None);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        let dropout_block = self.ctx.llvm_ctx.append_basic_block(func, "dropout");
        let conv_block = self.ctx.llvm_ctx.append_basic_block(func, "conv");

        self.ctx.builder.position_at_end(dropout_block);
        // TODO: RTE no such proc
        let trap_fn = self.get_intrinsic(Intrinsic::Trap);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        let mut cases = Vec::new();
        let mut phi_incoming = Vec::new();
        let mut proc_lookup_vals = Vec::new();
        for (i, proc_name) in ty.procs.iter().enumerate() {
            let case_block = self.ctx.llvm_ctx.append_basic_block(func, &format!("case_{}", proc_name.index()));
            self.ctx.builder.position_at_end(case_block);
            let disc_val = self.ctx.llvm_ctx.i64_type().const_int(proc_name.index() as u64, false);
            let proc_index_val = self.ctx.llvm_ctx.i32_type().const_int(i as u64, false);
            self.ctx.builder.build_unconditional_branch(conv_block);
            cases.push((disc_val, case_block));
            phi_incoming.push((proc_index_val, case_block));
            proc_lookup_vals.push(self.sym[ty.proc_lookup[proc_name].top_proc].as_global_value().as_pointer_value());
        }
        let proc_lookup_tbl_ty = ret_ty.array_type(cases.len() as u32);
        let proc_lookup_tbl_global = self.ctx.module.add_global(
            proc_lookup_tbl_ty,
            Some(inkwell::AddressSpace::Generic),
            &format!("ty_{}_proc_lookup_tbl", ty.id.index()));
        proc_lookup_tbl_global.set_initializer(&ret_ty.const_array(proc_lookup_vals.as_slice()));

        self.ctx.builder.position_at_end(entry_block);

        if cases.len() == 0 {
            // hack, TODO: replace
            self.ctx.builder.build_unconditional_branch(conv_block);
            self.ctx.builder.position_at_end(conv_block);
            self.ctx.builder.build_unconditional_branch(dropout_block);
            return func
        }

        let param_val = func.get_first_param().unwrap().into_int_value();
        self.ctx.builder.build_switch(param_val, dropout_block, &cases);

        self.ctx.builder.position_at_end(conv_block);
        let phi_incoming: Vec<_> = phi_incoming.iter().map(|(v, b)| (v as _, *b)).collect();
        let proc_index_val = self.ctx.builder.build_phi(self.ctx.llvm_ctx.i32_type(), "offset");
        proc_index_val.add_incoming(phi_incoming.as_slice());

        let proc_ptr_ptr = unsafe { self.ctx.builder.build_in_bounds_gep(
            proc_lookup_tbl_global.as_pointer_value(),
            &[
                self.ctx.llvm_ctx.i32_type().const_zero(),
                proc_index_val.as_basic_value().into_int_value(),
            ],
            "proc_ptr_ptr",
        ) };
        let proc_ptr = self.ctx.builder.build_load(proc_ptr_ptr, "proc_ptr");
        self.ctx.builder.build_return(Some(&proc_ptr));

        func
    }

    pub fn run(&self, opt: bool) {
        //self.ctx.module.print_to_stderr();
        self.dump_module("unopt");

        //let engine = self.ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

        if opt {
            let pm_builder = inkwell::passes::PassManagerBuilder::create();
            pm_builder.set_optimization_level(inkwell::OptimizationLevel::Aggressive);
            let pm = inkwell::passes::PassManager::create(());
            pm_builder.populate_module_pass_manager(&pm);
            pm.run_on(&self.ctx.module);

            //self.ctx.module.print_to_stderr();
            self.dump_module("opt");
        }

        /*unsafe {
            let func = engine.get_function::<unsafe extern "C" fn()>("main").unwrap();
            func.call();
        }*/
    }

    pub fn dump_module(&self, name: &str) {
        let buf = self.ctx.module.print_to_string().to_string();
        fs::create_dir_all("out/").unwrap();
        fs::write(format!("out/{}.ll", name), buf).unwrap();

        let success = self.ctx.module.write_bitcode_to_path(std::path::Path::new(&format!("out/{}.bc", name)));
        assert!(success);
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> inkwell::values::FunctionValue<'ctx> {
        // TODO: cache?
        let ty_f32 = self.ctx.llvm_ctx.f32_type().into();

        let (name, tys): (_, Vec<inkwell::types::BasicTypeEnum>) = match intrinsic {
            Intrinsic::FPow => ("llvm.pow", vec![ty_f32]),
            Intrinsic::Trap => ("llvm.trap", vec![]),
            //Intrinsic::LifetimeStart => ("llvm.lifetime.start", vec![]),
            //Intrinsic::LifetimeEnd => ("llvm.lifetime.end", vec![]),
            //Intrinsic::InvariantStart => ("llvm.invariant.start", vec![]),
        };

        unsafe { self.ctx.module.get_intrinsic(name, &tys).unwrap() }
    }
}

// TODO: move somewhere better
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Intrinsic {
    FPow,
    Trap,
    //LifetimeStart,
    //LifetimeEnd,
    //InvariantStart,
}
