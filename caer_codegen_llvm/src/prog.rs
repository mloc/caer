use std::fs::{self, File};

use caer_ir::cfg::*;
use caer_ir::module::Module;
use caer_ir::walker::CFGWalker;
use caer_types::id::{FuncId, InstanceTypeId, PathTypeId, StringId};
use caer_types::instance::InstanceType;
use caer_types::rt_env::RtEnvBundle;
use caer_types::type_tree::{PathType, Specialization};
use caer_types::{layout, ty};
use index_vec::IndexVec;
use inkwell::types::FunctionType;
use inkwell::values::{AnyValueEnum, FunctionValue, PointerValue};

use super::context::Context;
use super::func::FuncEmit;
use crate::context::GC_ADDRESS_SPACE;

#[derive(Debug)]
pub struct ProgEmit<'a, 'ctx> {
    pub ctx: &'a Context<'a, 'ctx>,
    pub env: &'a Module,
    pub funcs: Vec<(&'a Function, FunctionValue<'ctx>)>,
    pub rt_global: inkwell::values::GlobalValue<'ctx>,
    // vtable global
    pub vt_global: inkwell::values::GlobalValue<'ctx>,
    // Func table global
    pub ft_global: inkwell::values::GlobalValue<'ctx>,
    //pub vt_lookup: Vec<FunctionValue<'ctx>>,
    pub datum_types: IndexVec<InstanceTypeId, inkwell::types::StructType<'ctx>>,
    pub sym: IndexVec<FuncId, FunctionValue<'ctx>>,
    pub string_allocs: IndexVec<StringId, inkwell::values::PointerValue<'ctx>>,
}

impl<'a, 'ctx> ProgEmit<'a, 'ctx> {
    pub fn new(ctx: &'a Context<'a, 'ctx>, env: &'a Module) -> Self {
        let rt_global = ctx.module.add_global(
            ctx.rt.ty.rt_type,
            Some(inkwell::AddressSpace::Generic),
            "runtime",
        );
        rt_global.set_initializer(&ctx.rt.ty.rt_type.const_zero());

        // TODO: don't dig so deep into env?
        let vt_global_ty = ctx
            .rt
            .ty
            .vt_entry_type
            .array_type(env.type_tree.len() as u32);
        let vt_global =
            ctx.module
                .add_global(vt_global_ty, Some(inkwell::AddressSpace::Generic), "vtable");
        vt_global.set_constant(true);
        let ft_global_ty = ctx.rt.ty.opaque_type_ptr.array_type(env.funcs.len() as u32);
        let ft_global =
            ctx.module
                .add_global(ft_global_ty, Some(inkwell::AddressSpace::Generic), "ftable");
        ft_global.set_constant(true);

        Self {
            ctx,
            env,
            funcs: Vec::new(),
            rt_global,
            vt_global,
            ft_global,
            //vt_lookup: ctx.make_vtable_lookup(vt_global),
            datum_types: IndexVec::new(),
            sym: IndexVec::new(),
            string_allocs: IndexVec::new(),
        }
    }

    pub fn build_funcs(&mut self) {
        for func in self.env.funcs.iter() {
            self.add_func(func);
        }
    }

    // TODO: ERRH
    pub fn resolve_func(&self, id: FuncId) -> FunctionValue<'ctx> {
        self.sym[id]
    }

    pub fn lookup_global_proc(&self, name: StringId) -> FuncId {
        let global_dty = self.env.type_tree.global_type();
        global_dty.proc_lookup[&name].top_func
    }

    fn add_func(&mut self, ir_func: &'a Function) {
        //let mut proc_emit = ProcEmit::new(self.ctx, proc, name);
        let ty;
        if ir_func.closure.is_some() {
            ty = self.ctx.rt.ty.closure_type;
        } else {
            ty = self.ctx.rt.ty.proc_type;
        }

        let ll_func =
            self.ctx
                .module
                .add_function(&format!("proc_{}", ir_func.id.index()), ty, None);
        ll_func.set_personality_function(self.ctx.rt.dm_eh_personality);
        ll_func.set_gc("statepoint-example");

        assert_eq!(ir_func.id.index(), self.sym.len());
        self.sym.push(ll_func);
        self.funcs.push((ir_func, ll_func));
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
            let mut func_emit = FuncEmit::new(self.ctx, self, ir_func, ll_func);
            walker.walk(ir_func, &mut func_emit);
        }
        let main_proc = self.lookup_global_proc(self.env.intern_string_ro("entry"));

        self.finalize_main(main_block, main_proc);
    }

    fn emit_main(&mut self) -> inkwell::basic_block::BasicBlock<'ctx> {
        let func_type = self.ctx.llvm_ctx.i32_type().fn_type(&[], false);
        let func = self.ctx.module.add_function("main", func_type, None);

        self.ctx.llvm_ctx.append_basic_block(func, "entry")
    }

    pub(crate) fn copy_val(&self, src: PointerValue<'ctx>, dest: PointerValue<'ctx>) {
        let i8_ptr = self.ctx.llvm_ctx.i8_type().ptr_type(GC_ADDRESS_SPACE);

        let src_i8 = self.ctx.builder.build_bitcast(src, i8_ptr, "");
        let dest_i8 = self.ctx.builder.build_bitcast(dest, i8_ptr, "");

        let memcpy_intrinsic = unsafe {
            self.ctx
                .module
                .get_intrinsic(
                    "llvm.memcpy",
                    &[
                        i8_ptr.into(),
                        i8_ptr.into(),
                        self.ctx.llvm_ctx.i64_type().into(),
                    ],
                )
                .unwrap()
        };

        self.ctx.builder.build_call(
            memcpy_intrinsic,
            &[
                dest_i8,
                src_i8,
                self.ctx
                    .llvm_ctx
                    .i64_type()
                    .const_int(ty::Type::Any.get_store_size(), false)
                    .into(),
                self.ctx.llvm_ctx.bool_type().const_zero().into(),
            ],
            "",
        );
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
            self.ctx.rt.rt_runtime_init,
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
        for ty in self.env.type_tree.iter() {
            let vars_field_ty = self.ctx.rt.ty.val_type.array_type(ty.vars.len() as u32);
            let datum_ty = self.ctx.llvm_ctx.named_struct_type(
                &[
                    self.ctx.rt.ty.datum_common_type.into(),
                    vars_field_ty.into(),
                ],
                false,
                &format!("datum_{}", ty.id.index()),
            );
            assert_eq!(ty.id.index(), self.datum_types.len());
            self.datum_types.push(datum_ty);
        }
    }

    fn emit_string_table(&mut self) {
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
        self.string_allocs = string_globals
            .into_iter_enumerated()
            .map(|(id, (string_global, len))| {
                let alloc_string = self.ctx.rt.ty.string_type.const_named_struct(&[
                    self.ctx
                        .rt
                        .ty
                        .heap_header_type
                        .const_named_struct(&[
                            self.ctx.llvm_ctx.i8_type().const_int(1, false).into(),
                            self.ctx.llvm_ctx.i8_type().const_int(0, false).into(),
                        ])
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
                    self.ctx.rt.ty.string_type,
                    Some(GC_ADDRESS_SPACE),
                    &format!("alloc_string_{}", id.raw()),
                );
                asg.set_initializer(&alloc_string);
                asg.set_constant(true);
                asg.as_pointer_value()
            })
            .collect();
    }

    fn emit_ftable(&mut self) {
        let ft_entries: Vec<_> = self
            .sym
            .iter()
            .map(|fv| {
                fv.as_global_value()
                    .as_pointer_value()
                    .const_cast(self.ctx.rt.ty.opaque_type_ptr)
            })
            .collect();
        self.ft_global
            .set_initializer(&self.ctx.rt.ty.opaque_type_ptr.const_array(&ft_entries));
    }

    fn emit_vtable(&mut self) {
        let mut vt_entries = Vec::new();
        for instance in self.env.instances.iter() {
            let size_val = self.datum_types[instance.id]
                .size_of()
                .unwrap()
                .const_cast(self.ctx.llvm_ctx.i64_type(), false);
            let ity = self.env.instances.lookup_instance(instance.id).unwrap();
            let entries = match self.env.get_type_spec(instance.id) {
                Specialization::Datum => {
                    let var_index_fn = self.make_var_index_func(&ity.pty);
                    [
                        self.ctx
                            .llvm_ctx
                            .i32_type()
                            .const_int(ity.id.raw() as _, false)
                            .into(),
                        size_val.into(),
                        self.make_get_var_func(&ity, var_index_fn).into(),
                        self.make_set_var_func(&ity, var_index_fn).into(),
                        self.make_proc_lookup_func(&ity).into(),
                    ]
                },
                Specialization::List => [
                    self.ctx
                        .llvm_ctx
                        .i32_type()
                        .const_int(ity.id.raw() as _, false)
                        .into(),
                    size_val.into(),
                    self.ctx.rt.rt_list_var_get.into(),
                    self.ctx.rt.rt_list_var_set.into(),
                    self.ctx.rt.rt_list_proc_lookup.into(),
                ],
                Specialization::String => {
                    let field_tys = self.ctx.rt.ty.vt_entry_type.get_field_types();
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

            let field_tys = self.ctx.rt.ty.vt_entry_type.get_field_types();
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

            let vt_entry = self
                .ctx
                .rt
                .ty
                .vt_entry_type
                .const_named_struct(&cast_entries);
            vt_entries.push(vt_entry);
        }
        self.vt_global
            .set_initializer(&self.ctx.rt.ty.vt_entry_type.const_array(&vt_entries));
    }

    fn make_trapping_func(&mut self, ty: FunctionType<'ctx>, name: &str) -> FunctionValue<'ctx> {
        let func = self.ctx.module.add_function(name, ty, None);
        let trap_fn = self.get_intrinsic(Intrinsic::Trap);
        let block = self.ctx.llvm_ctx.append_basic_block(func, "trap");

        self.ctx.builder.position_at_end(block);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        func
    }

    fn make_var_index_func(&mut self, ty: &PathType) -> FunctionValue<'ctx> {
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
        let trap_fn = self.get_intrinsic(Intrinsic::Trap);
        self.ctx.builder.build_call(trap_fn, &[], "");
        self.ctx.builder.build_unreachable();

        let mut cases = Vec::new();
        let mut phi_incoming = Vec::new();
        for (i, var_name) in ty.vars.iter().enumerate() {
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
        &self, ty: &InstanceType, index_func: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.llvm_ctx.void_type().fn_type(
            &[
                datum_type_ptr.into(),
                self.ctx.llvm_ctx.i64_type().into(),
                self.ctx.rt.ty.val_type_ptr.into(),
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
        &self, ty: &InstanceType, index_func: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let datum_type_ptr = self.datum_types[ty.id].ptr_type(GC_ADDRESS_SPACE);
        let func_ty = self.ctx.llvm_ctx.void_type().fn_type(
            &[
                datum_type_ptr.into(),
                self.ctx.llvm_ctx.i64_type().into(),
                self.ctx.rt.ty.val_type_ptr.into(),
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
        self.copy_val(asg_val, var_val_ptr);
        self.ctx.builder.build_return(None);

        func
    }

    fn make_proc_lookup_func(&mut self, ty: &InstanceType) -> FunctionValue<'ctx> {
        let ret_ty = self
            .ctx
            .rt
            .ty
            .proc_type
            .ptr_type(inkwell::AddressSpace::Generic);
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
        let trap_fn = self.get_intrinsic(Intrinsic::Trap);
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
                self.sym[ty.pty.proc_lookup[proc_name].top_func]
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

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> FunctionValue<'ctx> {
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
