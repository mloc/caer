use std::fs::{self, File};
use std::ptr::NonNull;
use std::rc::Rc;

use caer_ir::cfg::*;
use caer_ir::module::Module;
use caer_ir::walker::CFGWalker;
use caer_runtime::arg_pack::ProcPack;
use caer_runtime::datum::Datum;
use caer_runtime::heap_object::{GcMarker, HeapHeader};
use caer_runtime::runtime::Runtime;
use caer_runtime::string::RtString;
use caer_runtime::val::Val;
use caer_runtime::vtable::{self, FuncPtr};
use caer_types::id::{FuncId, InstanceTypeId, StringId};
use caer_types::instance::InstanceType;
use caer_types::layout;
use caer_types::rt_env::RtEnvBundle;
use caer_types::ty::Type;
use caer_types::type_tree::{PathType, Specialization};
use index_vec::IndexVec;
use inkwell::types::{BasicType, FunctionType};
use inkwell::values::{AnyValueEnum, FunctionValue, PointerValue};

use super::context::Context;
use super::func::FuncEmit;
use crate::context::{ExFunc, GC_ADDRESS_SPACE};
use crate::symbol_table::SymbolTable;
use crate::type_manager::TypeManager;
use crate::value::BrandedValue;

//#[derive(Debug)]
//pub struct ProgCtx<'ctx> {}

#[derive(Debug)]
pub struct ProgEmit<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub env: &'ctx Module,
    pub funcs: Vec<(&'ctx Function, FunctionValue<'ctx>)>,
    pub rt_global: inkwell::values::GlobalValue<'ctx>,
    pub rt_global_val: BrandedValue<'ctx, *mut Runtime>,
    // vtable global
    pub vt_global: inkwell::values::GlobalValue<'ctx>,
    // Func table global
    pub ft_global: inkwell::values::GlobalValue<'ctx>,
    //pub vt_lookup: Vec<FunctionValue<'ctx>>,
    pub datum_types: IndexVec<InstanceTypeId, inkwell::types::StructType<'ctx>>,
    pub string_allocs: IndexVec<StringId, BrandedValue<'ctx, Option<NonNull<RtString>>>>,

    pub sym: SymbolTable<'ctx>,
    pub tys: TypeManager<'ctx>,
}

impl<'ctx> ProgEmit<'ctx> {
    pub fn new(ctx: &'ctx Context<'ctx>, env: &'ctx Module) -> Self {
        let tys = TypeManager::new(ctx);
        let sym = SymbolTable::new(ctx, &tys, env);

        Self {
            ctx,
            env,
            funcs: Vec::new(),
            rt_global: sym.rt_global,
            rt_global_val: sym.rt_global_val,
            vt_global: sym.vt_global,
            ft_global: sym.ft_global,
            //vt_lookup: ctx.make_vtable_lookup(vt_global),
            datum_types: IndexVec::new(),
            string_allocs: IndexVec::new(),

            sym,
            tys,
        }
    }

    // TODO: ERRH
    pub fn resolve_func(&self, id: FuncId) -> FunctionValue<'ctx> {
        self.sym.get_ir_llvm_func(id).unwrap()
    }

    pub fn emit(&mut self) {
        self.populate_datum_types();
        self.emit_vtable();
        self.emit_ftable();
        self.emit_string_table();
        let main_block = self.emit_main();

        for (ir_func, ll_func) in self.funcs.drain(..).collect::<Vec<_>>() {
            // TODO no
            println!("EMITTING {:?}", ir_func.id);
            let walker = CFGWalker::build(ir_func);
            let ctx = self.ctx.clone();
            let mut func_emit = FuncEmit::new(&ctx, self.env, &self.sym, ir_func, ll_func);
            walker.walk(ir_func, &mut func_emit);
        }
        let main_proc = self
            .sym
            .lookup_global_proc(self.env.intern_string_ro("entry"));

        self.finalize_main(main_block, main_proc);
    }

    fn emit_main(&mut self) -> inkwell::basic_block::BasicBlock<'ctx> {
        let func_type = self.ctx.llvm_ctx.i32_type().fn_type(&[], false);
        let func = self.ctx.module.add_function("main", func_type, None);

        self.ctx.llvm_ctx.append_basic_block(func, "entry")
    }

    fn finalize_main(&self, block: inkwell::basic_block::BasicBlock, entry_func: FuncId) {
        self.ctx.builder.position_at_end(block);

        let func_specs = self.env.funcs.iter().map(|f| f.get_spec()).collect();

        // TODO: move file emit to a better place
        let rt_env = RtEnvBundle {
            string_table: self.env.string_table.clone().freeze(),
            type_tree: self.env.type_tree.clone(),
            instances: self.env.instances.clone(),
            func_specs,
            types: self.env.types.get_all(),
        };
        bincode::serialize_into(File::create("environment.bincode").unwrap(), &rt_env).unwrap();

        // not used by runtime, just for debugging / aux tooling
        serde_json::to_writer_pretty(
            File::create("stringtable.json").unwrap(),
            &self.env.string_table,
        )
        .unwrap();
        serde_json::to_writer_pretty(File::create("environment.json").unwrap(), &rt_env).unwrap();

        let vt_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                self.vt_global.as_pointer_value(),
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                ],
                "vt_ptr",
            )
        };
        let ft_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                self.ft_global.as_pointer_value(),
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                ],
                "ft_ptr",
            )
        };

        // defined in linker script
        let stackmap_start =
            self.ctx
                .module
                .add_global(self.ctx.llvm_ctx.i8_type(), None, "__stackmaps_start");
        stackmap_start.set_linkage(inkwell::module::Linkage::External);
        let stackmap_end =
            self.ctx
                .module
                .add_global(self.ctx.llvm_ctx.i8_type(), None, "__stackmaps_end");
        stackmap_end.set_linkage(inkwell::module::Linkage::External);

        self.ctx.builder.build_call(
            self.sym.get_func(ExFunc::rt_runtime_init),
            &[
                self.rt_global.as_pointer_value().into(),
                stackmap_start.as_pointer_value().into(),
                stackmap_end.as_pointer_value().into(),
                vt_ptr.into(),
                ft_ptr.into(),
                self.ctx
                    .llvm_ctx
                    .i64_type()
                    .const_int(entry_func.raw(), false)
                    .into(),
            ],
            "",
        );

        self.ctx
            .builder
            .build_return(Some(&self.ctx.llvm_ctx.i32_type().const_int(0, false)));
    }

    fn populate_datum_types(&mut self) {
        for ty in self.env.instances.iter() {
            let vars_field_ty = self
                .tys
                .get_llvm_type::<Val>()
                .array_type(ty.pty.vars.len() as u32);
            let datum_ty = self
                .ctx
                .llvm_ctx
                .opaque_struct_type(&format!("datum_{}", ty.id.index()));
            datum_ty.set_body(
                &[self.tys.get_llvm_type::<Datum>(), vars_field_ty.into()],
                false,
            );
            assert_eq!(ty.id.index(), self.datum_types.len());
            self.datum_types.push(datum_ty);
        }
    }

    fn emit_string_table(&mut self) {
        let string_repr = self.tys.get_struct::<RtString>();
        let hh_repr = self.tys.get_struct::<HeapHeader>();
        let gcm_repr = self.tys.get_enum::<GcMarker>();

        let string_globals: IndexVec<StringId, _> = self
            .env
            .string_table
            .iter()
            .map(|(id, s)| {
                let cs = self.ctx.llvm_ctx.const_string(s.as_bytes(), false);
                let g = self.ctx.module.add_global(
                    cs.get_type(),
                    None,
                    &format!("string_{}", id.raw()),
                );
                g.set_initializer(&cs);
                g.set_constant(true);
                (g, s.len())
            })
            .collect();

        // This is split into two passes to make the resulting IR look a bit nicer
        // TODO: build with nicer pinion types
        self.string_allocs = string_globals
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
                    self.ctx
                        .llvm_ctx
                        .i64_type()
                        .const_int(len as _, false)
                        .into(),
                    unsafe {
                        self.ctx.builder.build_in_bounds_gep(
                            string_global.as_pointer_value(),
                            &[
                                self.ctx.llvm_ctx.i32_type().const_zero(),
                                self.ctx.llvm_ctx.i32_type().const_zero(),
                            ],
                            "",
                        )
                    }
                    .into(),
                ]);
                let asg = self.ctx.module.add_global(
                    string_repr.ty,
                    Some(GC_ADDRESS_SPACE),
                    &format!("alloc_string_{}", id.raw()),
                );
                asg.set_initializer(&alloc_string);
                asg.set_constant(true);
                unsafe { BrandedValue::materialize(&self.ctx, asg.as_pointer_value().into()) }
            })
            .collect();
    }

    fn emit_ftable(&mut self) {
        let funcptr_ty = self.tys.get_llvm_type::<FuncPtr>().into_pointer_type();

        let ft_entries: Vec<_> = self
            .sym
            .iter_ir_llvm_funcs()
            .map(|(_, fv)| {
                fv.as_global_value()
                    .as_pointer_value()
                    .const_cast(funcptr_ty)
            })
            .collect();
        self.ft_global
            .set_initializer(&funcptr_ty.const_array(&ft_entries));
    }

    fn emit_vtable(&mut self) {
        let vt_entry_ty = self.tys.get_struct::<vtable::Entry>().ty;
        let mut vt_entries = Vec::new();
        for instance in self.env.instances.iter() {
            let size_val = self.datum_types[instance.id]
                .size_of()
                .unwrap()
                .const_cast(self.ctx.llvm_ctx.i64_type(), false);
            let ity = self.env.instances.lookup_instance(instance.id).unwrap();
            let field_tys = vt_entry_ty.get_field_types();
            let entries = match self.env.get_type_spec(instance.id) {
                Specialization::Datum => {
                    let var_index_fn = self.make_var_index_func(ity);
                    [
                        self.ctx
                            .llvm_ctx
                            .i32_type()
                            .const_int(ity.id.raw() as _, false)
                            .into(),
                        size_val.into(),
                        self.make_get_var_func(ity, var_index_fn).into(),
                        self.make_set_var_func(ity, var_index_fn).into(),
                        self.make_proc_lookup_func(ity).into(),
                    ]
                },
                Specialization::List => [
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(ity.id.raw() as _, false)
                        .into(),
                    size_val.into(),
                    self.sym.get_func(ExFunc::rt_list_var_get).into(),
                    self.sym.get_func(ExFunc::rt_list_var_set).into(),
                    self.sym.get_func(ExFunc::rt_list_proc_lookup).into(),
                ],
                Specialization::String => {
                    [
                        self.ctx
                            .llvm_ctx
                            .i32_type()
                            .const_int(ity.id.raw() as _, false)
                            .into(),
                        self.ctx
                            .llvm_ctx
                            .i64_type()
                            .const_int(1 << 60, false)
                            .into(), // lol
                        self.make_trapping_func(
                            field_tys[2]
                                .into_pointer_type()
                                .get_element_type()
                                .into_function_type(),
                            &format!("ty_{}_var_get_STRTRAP", ity.id.index()),
                        )
                        .into(),
                        self.make_trapping_func(
                            field_tys[3]
                                .into_pointer_type()
                                .get_element_type()
                                .into_function_type(),
                            &format!("ty_{}_var_set_STRTRAP", ity.id.index()),
                        )
                        .into(),
                        self.make_trapping_func(
                            field_tys[4]
                                .into_pointer_type()
                                .get_element_type()
                                .into_function_type(),
                            &format!("ty_{}_proc_lookup_get_STRTRAP", ity.id.index()),
                        )
                        .into(),
                    ]
                },
                x => todo!("{:?}", x),
            };

            assert_eq!(entries.len(), field_tys.len());
            let cast_entries: Vec<_> = entries
                .iter()
                .enumerate()
                .map(|(i, entry)| match *entry {
                    AnyValueEnum::FunctionValue(f) => self.ctx.builder.build_bitcast(
                        f.as_global_value().as_pointer_value(),
                        field_tys[i],
                        "",
                    ),
                    AnyValueEnum::IntValue(v) => v.into(),
                    _ => unimplemented!("add handler"),
                })
                .collect();

            let vt_entry = vt_entry_ty.const_named_struct(&cast_entries);
            vt_entries.push(vt_entry);
        }
        self.vt_global
            .set_initializer(&vt_entry_ty.const_array(&vt_entries));
    }

    fn make_trapping_func(&mut self, ty: FunctionType<'ctx>, name: &str) -> FunctionValue<'ctx> {
        let func = self.ctx.module.add_function(name, ty, None);
        let trap_fn = self.ctx.get_intrinsic(Intrinsic::Trap);
        let block = self.ctx.llvm_ctx.append_basic_block(func, "trap");

        self.ctx.builder.position_at_end(block);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        func
    }

    fn make_var_index_func(&mut self, ty: &InstanceType) -> FunctionValue<'ctx> {
        let func_ty = self
            .ctx
            .llvm_ctx
            .i32_type()
            .fn_type(&[self.ctx.llvm_ctx.i64_type().into()], false);
        // TODO: MANGLE
        let func =
            self.ctx
                .module
                .add_function(&format!("ty_{}_var_index", ty.id.index()), func_ty, None);
        // TODO: revisit
        let attr = self
            .ctx
            .llvm_ctx
            .create_string_attribute("alwaysinline", "");
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
        let trap_fn = self.ctx.get_intrinsic(Intrinsic::Trap);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        let mut cases = Vec::new();
        let mut phi_incoming = Vec::new();
        for (i, var_name) in ty.pty.vars.iter().enumerate() {
            let case_block = self
                .ctx
                .llvm_ctx
                .append_basic_block(func, &format!("case_{}", var_name.index()));
            self.ctx.builder.position_at_end(case_block);
            let disc_val = self
                .ctx
                .llvm_ctx
                .i64_type()
                .const_int(var_name.index() as u64, false);
            let this_offset_val = self.ctx.llvm_ctx.i32_type().const_int(i as u64, false);
            self.ctx.builder.build_unconditional_branch(conv_block);
            cases.push((disc_val, case_block));
            phi_incoming.push((this_offset_val, case_block));
        }

        self.ctx.builder.position_at_end(entry_block);

        if cases.is_empty() {
            // hack, TODO: replace
            self.ctx.builder.build_unconditional_branch(conv_block);
            self.ctx.builder.position_at_end(conv_block);
            self.ctx.builder.build_unconditional_branch(dropout_block);
            return func;
        }

        let param_val = func.get_first_param().unwrap().into_int_value();
        self.ctx
            .builder
            .build_switch(param_val, dropout_block, &cases);

        self.ctx.builder.position_at_end(conv_block);
        let phi_incoming: Vec<_> = phi_incoming.iter().map(|(v, b)| (v as _, *b)).collect();
        let offset_val = self
            .ctx
            .builder
            .build_phi(self.ctx.llvm_ctx.i32_type(), "offset");
        offset_val.add_incoming(phi_incoming.as_slice());
        self.ctx
            .builder
            .build_return(Some(&offset_val.as_basic_value()));

        func
    }

    fn make_get_var_func(
        &mut self, ty: &InstanceType, index_func: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.llvm_ctx.void_type().fn_type(
            &[
                datum_type_ptr.into(),
                self.ctx.llvm_ctx.i64_type().into(),
                self.tys.get_llvm_type_ptr::<Val>().into(),
            ],
            false,
        );
        // TODO: MANGLE
        let func =
            self.ctx
                .module
                .add_function(&format!("ty_{}_var_get", ty.id.index()), func_ty, None);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(entry_block);

        let datum_ptr = func.get_params()[0].into_pointer_value();
        let var_name = func.get_params()[1].into_int_value();
        let dest_ptr = func.get_params()[2].into_pointer_value();

        let var_index = self
            .ctx
            .builder
            .build_call(index_func, &[var_name.into()], "var_index")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        let var_val_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                datum_ptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(layout::DATUM_VARS_FIELD_OFFSET, false),
                    var_index,
                ],
                "var_val_ptr",
            )
        };
        let var_val = self.ctx.builder.build_load(var_val_ptr, "var_val");
        self.ctx.builder.build_store(dest_ptr, var_val);
        self.ctx.builder.build_return(None);

        func
    }

    fn make_set_var_func(
        &mut self, ty: &InstanceType, index_func: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.llvm_ctx.void_type().fn_type(
            &[
                datum_type_ptr.into(),
                self.ctx.llvm_ctx.i64_type().into(),
                self.tys.get_llvm_type_ptr::<Val>().into(),
            ],
            false,
        );
        // TODO: MANGLE
        let func =
            self.ctx
                .module
                .add_function(&format!("ty_{}_var_set", ty.id.index()), func_ty, None);

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(entry_block);

        let datum_ptr = func.get_params()[0].into_pointer_value();
        let var_name = func.get_params()[1].into_int_value();
        let asg_val = func.get_params()[2].into_pointer_value();

        let var_index = self
            .ctx
            .builder
            .build_call(index_func, &[var_name.into()], "var_index")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        let var_val_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                datum_ptr,
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(layout::DATUM_VARS_FIELD_OFFSET, false),
                    var_index,
                ],
                "var_val_ptr",
            )
        };
        self.ctx.copy_val(asg_val, var_val_ptr);
        self.ctx.builder.build_return(None);

        func
    }

    fn make_proc_lookup_func(&mut self, ty: &InstanceType) -> FunctionValue<'ctx> {
        let ret_ty = self.sym.proc_type.ptr_type(inkwell::AddressSpace::Generic);
        let func_ty = ret_ty.fn_type(&[self.ctx.llvm_ctx.i64_type().into()], false);
        let func = self.ctx.module.add_function(
            &format!("ty_{}_proc_lookup", ty.id.index()),
            func_ty,
            None,
        );

        let entry_block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        let dropout_block = self.ctx.llvm_ctx.append_basic_block(func, "dropout");
        let conv_block = self.ctx.llvm_ctx.append_basic_block(func, "conv");

        self.ctx.builder.position_at_end(dropout_block);
        // TODO: RTE no such proc
        let trap_fn = self.ctx.get_intrinsic(Intrinsic::Trap);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        let mut cases = Vec::new();
        let mut phi_incoming = Vec::new();
        let mut proc_lookup_vals = Vec::new();
        for (i, proc_name) in ty.pty.procs.iter().enumerate() {
            let case_block = self
                .ctx
                .llvm_ctx
                .append_basic_block(func, &format!("case_{}", proc_name.index()));
            self.ctx.builder.position_at_end(case_block);
            let disc_val = self
                .ctx
                .llvm_ctx
                .i64_type()
                .const_int(proc_name.index() as u64, false);
            let proc_index_val = self.ctx.llvm_ctx.i32_type().const_int(i as u64, false);
            self.ctx.builder.build_unconditional_branch(conv_block);
            cases.push((disc_val, case_block));
            phi_incoming.push((proc_index_val, case_block));
            proc_lookup_vals.push(
                self.resolve_func(ty.pty.proc_lookup[proc_name].top_func)
                    .as_global_value()
                    .as_pointer_value(),
            );
        }
        let proc_lookup_tbl_ty = ret_ty.array_type(cases.len() as u32);
        let proc_lookup_tbl_global = self.ctx.module.add_global(
            proc_lookup_tbl_ty,
            Some(inkwell::AddressSpace::Generic),
            &format!("ty_{}_proc_lookup_tbl", ty.id.index()),
        );
        proc_lookup_tbl_global.set_constant(true);
        proc_lookup_tbl_global.set_initializer(&ret_ty.const_array(proc_lookup_vals.as_slice()));

        self.ctx.builder.position_at_end(entry_block);

        if cases.is_empty() {
            // hack, TODO: replace
            self.ctx.builder.build_unconditional_branch(conv_block);
            self.ctx.builder.position_at_end(conv_block);
            self.ctx.builder.build_unconditional_branch(dropout_block);
            return func;
        }

        let param_val = func.get_first_param().unwrap().into_int_value();
        self.ctx
            .builder
            .build_switch(param_val, dropout_block, &cases);

        self.ctx.builder.position_at_end(conv_block);
        let phi_incoming: Vec<_> = phi_incoming.iter().map(|(v, b)| (v as _, *b)).collect();
        let proc_index_val = self
            .ctx
            .builder
            .build_phi(self.ctx.llvm_ctx.i32_type(), "offset");
        proc_index_val.add_incoming(phi_incoming.as_slice());

        let proc_ptr_ptr = unsafe {
            self.ctx.builder.build_in_bounds_gep(
                proc_lookup_tbl_global.as_pointer_value(),
                &[
                    self.ctx.llvm_ctx.i32_type().const_zero(),
                    proc_index_val.as_basic_value().into_int_value(),
                ],
                "proc_ptr_ptr",
            )
        };
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

        let success = self
            .ctx
            .module
            .write_bitcode_to_path(std::path::Path::new(&format!("out/{}.bc", name)));
        assert!(success);
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
