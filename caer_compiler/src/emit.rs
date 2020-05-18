use indexed_vec::{IndexVec, Idx};
use std::collections::HashMap;
use crate::cfg::*;
use std::fs;
use std::borrow::Borrow;
use crate::ty::{self, Ty};
use std::convert::TryInto;
use std::mem::size_of;

struct Value<'a> {
    val: Option<inkwell::values::BasicValueEnum<'a>>,
    ty: ty::Complex,
}

impl<'a> Value<'a> {
    fn new(val: Option<inkwell::values::BasicValueEnum<'a>>, ty: ty::Complex) -> Self {
        if val == None && ty == ty::Primitive::Null.into() {
            panic!("values with non-null ty must have a val")
        }
        Self {
            val: val,
            ty: ty,
        }
    }
}

#[derive(Debug)]
struct ProcEmit<'a, 'ctx> {
    ctx: &'a Context<'a, 'ctx>,
    emit: &'a Emit<'a, 'ctx>,
    local_allocs: IndexVec<LocalId, inkwell::values::PointerValue<'ctx>>,
    blocks: IndexVec<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    func: inkwell::values::FunctionValue<'ctx>,
    proc: &'a Proc,
    name: String,
}

impl<'a, 'ctx> ProcEmit<'a, 'ctx> {
    fn new(ctx: &'a Context<'a, 'ctx>, emit: &'a Emit<'a, 'ctx>, proc: &'a Proc, func: inkwell::values::FunctionValue<'ctx>, name: &str) -> Self {
        Self {
            ctx: ctx,
            emit: emit,
            local_allocs: IndexVec::new(),
            blocks: IndexVec::new(),
            func: func,
            proc: proc,
            name: name.to_string(),
        }
    }

    fn emit_entry_block(&mut self) -> inkwell::basic_block::BasicBlock<'a> {
        let block = self.ctx.llvm_ctx.append_basic_block(self.func, "entry");
        self.ctx.builder.position_at_end(block);

        let null_val = self.ctx.rt.ty.val_type.const_zero();

        for local in self.proc.locals.iter() {
            let name = match local.name {
                Some(ref s) => s,
                None => "",
            };

            let alloc = match local.ty {
                ty::Complex::Primitive(prim) => {
                    match prim {
                        ty::Primitive::Float => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.f32_type(), name),
                        ty::Primitive::String => self.ctx.builder.build_alloca(self.ctx.llvm_ctx.i64_type(), name),
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

        block
    }

    fn finalize_entry_block(&self, entry: inkwell::basic_block::BasicBlock) {
        self.ctx.builder.position_at_end(entry);
        self.ctx.builder.build_unconditional_branch(self.blocks[BlockId::new(0)]);
    }

    fn emit_proc(&mut self, emit: &Emit) {
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
                /*
                // TODO make this all better, when inkwell supports GEP directly on const_string
                // this GEPpery is bad.
                let const_str = self.ctx.llvm_ctx.const_string(s.as_bytes(), false);
                let tmp_local = self.ctx.builder.build_alloca(const_str.get_type(), "tmp_str");//.const_cast(self.ctx.llvm_ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic));
                self.ctx.builder.build_store(tmp_local, const_str);

                let cz = self.ctx.llvm_ctx.i32_type().const_int(0, false);
                let tmp_local_ptr = unsafe { self.ctx.builder.build_gep(tmp_local, &[cz, cz], "") };
                let len_val = self.ctx.llvm_ctx.i32_type().const_int(s.len().try_into().unwrap(), false);

                let rt_local = self.ctx.builder.build_load(self.emit.rt_global, "local_runtime");
                let lit_val = self.ctx.builder.build_call(self.ctx.rt.rt_string_from_utf8, &[rt_local, tmp_local_ptr.into(), len_val.into()], "dmstr_id").try_as_basic_value().left().unwrap().into();
                */

                let lit_val = self.ctx.llvm_ctx.i64_type().const_int(*id, false).into();

                Value::new(Some(lit_val), ty::Primitive::String.into())
            },
            Literal::Null => {
                Value::new(None, ty::Primitive::Null.into())
            },
            _ => unimplemented!("{:?}", lit),
        };

        self.store_local(local_id, &val);
    }

    fn load_local<L: Borrow<LocalId>>(&self, local: L) -> Value {
        let ty = self.proc.locals[*local.borrow()].ty.clone();
        let val = self.ctx.builder.build_load(self.local_allocs[*local.borrow()], "").into();

        Value::new(val, ty)
    }

    fn store_local<L: Borrow<LocalId>>(&self, local_id: L, val: &Value) {
        let local_id = *local_id.borrow();
        let local_ty = &self.proc.locals[local_id].ty;
        let local_ptr = self.local_allocs[local_id];
        self.store_val(val, local_ty, local_ptr);
    }

    fn convert_to_any(&self, local_id: LocalId) -> inkwell::values::PointerValue {
        let local = &self.proc.locals[local_id];

        if local.ty == ty::Complex::Any {
            return self.local_allocs[local_id];
        }

        let val = self.load_local(local_id);
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

    fn emit_block(&self, block: &Block, emit: &Emit) {
        for op in block.ops.iter() {
            match op {
                Op::Literal(id, literal) => {
                    self.assign_literal(literal, *id);
                },

                Op::MkVar(_) => {
                    // nothing now - can we remove this?
                }

                Op::Load(local, var) => {
                    let copy = self.load_local(var);
                    self.store_local(local, &copy);
                    if copy.ty.needs_destructor() {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*local].into()], "");
                    }
                },

                Op::Store(var, local) => {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[self.local_allocs[*var].into()], "");

                    let copy = self.load_local(local);
                    self.store_local(var, &copy);

                    if copy.ty.needs_destructor() && self.proc.locals[*local].movable {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*var].into()], "");
                    }
                },

                Op::Put(id) => {
                    let norm = self.convert_to_any(*id);

                    let rt_local = self.ctx.builder.build_load(self.emit.rt_global, "local_runtime");
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &[norm.into(), rt_local], "put");
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.convert_to_any(*lhs).into();
                    let rhs_ref = self.convert_to_any(*rhs).into();
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false).into();

                    let rt_local = self.ctx.builder.build_load(self.emit.rt_global, "local_runtime");
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_binary_op, &[self.local_allocs[*id].into(), rt_local, op_var, lhs_ref, rhs_ref], "");
                },

                Op::Call(id, name, args) => {
                    assert!(args.len() == 0);
                    let func = emit.sym[name];
                    let res_val = self.ctx.builder.build_call(func, &[], name).try_as_basic_value().left().unwrap();
                    let val = Value::new(Some(res_val), ty::Complex::Any);
                    self.store_local(id, &val);
                },

                Op::Cast(dst, src, ty) => {
                    if *ty != ty::Primitive::String {
                        unimplemented!("can only cast to string, not {:?}", ty);
                    }

                    let src_ref = self.convert_to_any(*src).into();
                    let rt_local = self.ctx.builder.build_load(self.emit.rt_global, "local_runtime");
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_cast_string_val, &[self.local_allocs[*dst].into(), src_ref, rt_local], "");
                }

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

#[derive(Debug)]
pub struct Emit<'a, 'ctx> {
    ctx: &'a Context<'a, 'ctx>,
    env: &'a Environment,
    procs: Vec<(&'a Proc, &'a str, inkwell::values::FunctionValue<'ctx>)>,
    rt_global: inkwell::values::PointerValue<'ctx>,
    sym: HashMap<String, inkwell::values::FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Emit<'a, 'ctx> {
    pub fn new(ctx: &'a Context<'a, 'ctx>, env: &'a Environment) -> Self {
        let oppty = ctx.rt.ty.opaque_type.ptr_type(inkwell::AddressSpace::Generic);
        let rt_global = ctx.module.add_global(oppty, Some(inkwell::AddressSpace::Generic), "runtime");
        rt_global.set_initializer(&oppty.const_null());
        Self {
            ctx: ctx,
            env: env,
            rt_global: rt_global.as_pointer_value(),
            procs: Vec::new(),
            sym: HashMap::new(),
        }
    }

    pub fn build_procs(&mut self) {
        for (name, proc) in self.env.procs.iter() {
            self.add_proc(&name, &proc);
        }
    }

    fn add_proc(&mut self, name: &'a str, proc: &'a Proc) {
        //let mut proc_emit = ProcEmit::new(self.ctx, proc, name);
        let func_type = self.ctx.rt.ty.val_type.fn_type(&[], false);
        let func = self.ctx.module.add_function(name, func_type, None);

        self.sym.insert(name.to_string(), func);
        self.procs.push((proc, name, func));
    }

    pub fn emit(&mut self) {
        let main_block = self.emit_main();

        let mut main_proc = None;
        for (proc, name, func) in self.procs.drain(..).collect::<Vec<_>>() { // TODO no
            let mut proc_emit = ProcEmit::new(self.ctx, self, proc, func, name);
            proc_emit.emit_proc(self);
            if proc_emit.name == "entry" {
                main_proc = Some(proc_emit.func.clone());
            }
        }

        self.finalize_main(main_block, main_proc.unwrap());
    }

    fn emit_main(&mut self) -> inkwell::basic_block::BasicBlock<'ctx> {
        let func_type = self.ctx.llvm_ctx.void_type().fn_type(&[], false);
        let func = self.ctx.module.add_function("main", func_type, None);

        let block = self.ctx.llvm_ctx.append_basic_block(func, "entry");
        self.ctx.builder.position_at_end(block);


        // TODO make this all better, when inkwell supports GEP directly on const_string
        // this GEPpery is bad.
        let st_init_ser = self.env.string_table.serialize();
        let st_init_const = self.ctx.llvm_ctx.const_string(&st_init_ser, false);
        let tmp_local = self.ctx.builder.build_alloca(st_init_const.get_type(), "tmp_st_init");
        self.ctx.builder.build_store(tmp_local, st_init_const);

        let cz = self.ctx.llvm_ctx.i32_type().const_int(0, false);
        let tmp_local_ptr = unsafe { self.ctx.builder.build_gep(tmp_local, &[cz, cz], "") };
        let len_val = self.ctx.llvm_ctx.i64_type().const_int(st_init_ser.len().try_into().unwrap(), false);

        let rt_ptr = self.ctx.builder.build_call(self.ctx.rt.rt_runtime_init, &[tmp_local_ptr.into(), len_val.into()], "runtime").try_as_basic_value().left().unwrap().into_pointer_value();
        self.ctx.builder.build_store(self.rt_global, rt_ptr);

        block
    }

    fn finalize_main(&self, block: inkwell::basic_block::BasicBlock, entry_func: inkwell::values::FunctionValue) {
        self.ctx.builder.position_at_end(block);
        self.ctx.builder.build_call(entry_func, &[], "");
        self.ctx.builder.build_return(None);
    }

    pub fn run(&self, opt: bool) {
        //self.ctx.module.print_to_stderr();
        self.dump_module("unopt");

        let engine = self.ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

        if opt {
            let pm_builder = inkwell::passes::PassManagerBuilder::create();
            pm_builder.set_optimization_level(inkwell::OptimizationLevel::Aggressive);
            let pm = inkwell::passes::PassManager::create(());
            pm_builder.populate_module_pass_manager(&pm);
            pm.run_on(&self.ctx.module);

            //self.ctx.module.print_to_stderr();
            self.dump_module("opt");
        }

        unsafe {
            let func = engine.get_function::<unsafe extern "C" fn()>("main").unwrap();
            func.call();
        }
    }

    pub fn dump_module(&self, name: &str) {
        let buf = self.ctx.module.print_to_string().to_string();
        fs::create_dir_all("dbgout/llvm/").unwrap();
        fs::write(format!("dbgout/llvm/{}.ll", name), buf).unwrap();
    }

}

#[derive(Debug)]
pub struct Context<'a, 'ctx> {
    llvm_ctx: &'ctx inkwell::context::Context,
    builder: &'a inkwell::builder::Builder<'ctx>,
    module: &'a inkwell::module::Module<'ctx>,
    rt: RtFuncs<'ctx>,
}

impl<'a, 'ctx> Context<'a, 'ctx> {
    pub fn new(llctx: &'ctx inkwell::context::Context, llmod: &'a inkwell::module::Module<'ctx>, llbuild: &'a inkwell::builder::Builder<'ctx>) -> Self {
        let rt = RtFuncs::new(llctx, llmod);

        Self {
            builder: llbuild,
            module: llmod,
            llvm_ctx: llctx,
            rt: rt,
        }
    }
}

#[derive(Debug)]
struct RtFuncTyBundle<'ctx> {
    // TODO: undo this, it's a hack to clean up optimized output
    val_type: inkwell::types::StructType<'ctx>,
    //val_type: inkwell::types::IntType<'ctx>,
    opaque_type: inkwell::types::StructType<'ctx>,
}

macro_rules! rt_funcs {
    ( $name:ident, [ $( ( $func:ident, $ret:ident ~ $retspec:ident, [ $( $arg:ident ~ $argspec:ident ),* $(,)* ] ) ),* $(,)* ] ) => {
        #[derive(Debug)]
        struct $name <'ctx> {
            ty: RtFuncTyBundle<'ctx>,
            $(
                $func: inkwell::values::FunctionValue<'ctx>,
            )*
        }

        impl<'ctx> $name <'ctx> {
            fn new(ctx: &'ctx inkwell::context::Context, module: &inkwell::module::Module<'ctx>) -> $name<'ctx> {
                let padding_size = size_of::<ludo::val::Val>() - 4; // u32 discrim
                // TODO: undo this, it's a hack to clean up optimized output
                let val_padding_type = ctx.i32_type();
                let val_type = ctx.struct_type(&[ctx.i32_type().into(), val_padding_type.into(), ctx.i64_type().into()], true);
                // safety net for me.
                assert_eq!(padding_size, 12);
                //let val_type = ctx.i64_type();
                let val_ptr_type = val_type.ptr_type(inkwell::AddressSpace::Generic);
                let val_type = val_type.into();

                let opaque_type = ctx.opaque_struct_type("opaque");

                let tyb = RtFuncTyBundle {
                    val_type: val_type,
                    opaque_type: opaque_type,
                };

                $name {
                    $(
                        $func: module.add_function(stringify!($func),
                            rt_funcs!(@genty ctx $retspec tyb $ret $ret).fn_type(&[
                                $(
                                    rt_funcs!(@genty ctx $argspec tyb $arg $arg).into(),
                                )*
                            ], false),
                        None),
                    )*
                    ty: tyb,
                }
            }
        }
    };

    ( @genty $ctx:ident $spec:ident $tyb:ident val_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident opaque_type $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $tyb.$ty)
    );

    ( @genty $ctx:ident $spec:ident $tyb:ident $tym:ident $ty:ident) => (
        rt_funcs!(@genty @ptrify $spec , $ctx.$ty())
    );

    ( @genty @ptrify val , $e:expr) => (
        $e
    );

    ( @genty @ptrify ptr , $e:expr) => (
        $e.ptr_type(inkwell::AddressSpace::Generic)
    );
}

rt_funcs!{
    RtFuncs,
    [
        (rt_val_float, void_type~val, [val_type~ptr, f32_type~val]),
        (rt_val_string, void_type~val, [val_type~ptr, i64_type~val]),
        //(rt_val_int, void_type, [val_ptr_type, i32_type]),
        (rt_val_binary_op, void_type~val, [val_type~ptr, opaque_type~ptr, i32_type~val, val_type~ptr, val_type~ptr]),
        (rt_val_to_switch_disc, i32_type~val, [val_type~ptr]),
        (rt_val_print, void_type~val, [val_type~ptr, opaque_type~ptr]),
        (rt_val_cloned, void_type~val, [val_type~ptr]),
        (rt_val_drop, void_type~val, [val_type~ptr]),
        (rt_val_cast_string_val, void_type~val, [val_type~ptr, val_type~ptr, opaque_type~ptr]),

        (rt_string_from_utf8, i64_type~val, [opaque_type~ptr, i8_type~ptr, i32_type~val]),

        (rt_runtime_init, opaque_type~ptr, [i8_type~ptr, i64_type~val]),

        (rt_arg_pack_unpack_into, void_type~val, [opaque_type~ptr, val_type~ptr, i64_type~val, opaque_type~ptr]),
    ]
}
