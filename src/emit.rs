use indexed_vec::{IndexVec, Idx};
use std::collections::HashMap;
use crate::cfg::*;
use std::fs;
use std::borrow::Borrow;
use ludo;
use ludo::ty::Ty;
use std::mem::size_of;

#[derive(Debug)]
struct ProcEmit<'a> {
    ctx: &'a Context,
    local_allocs: IndexVec<LocalId, inkwell::values::PointerValue>,
    blocks: IndexVec<BlockId, inkwell::basic_block::BasicBlock>,
    func: inkwell::values::FunctionValue,
    proc: &'a Proc,
    name: String,
}

impl<'a> ProcEmit<'a> {
    fn new(ctx: &'a Context, proc: &'a Proc, name: &str) -> Self {
        let func_type = ctx.rt.val_type.fn_type(&[], false);
        let func = ctx.module.add_function(name, func_type, None);

        Self {
            ctx: ctx,
            local_allocs: IndexVec::new(),
            blocks: IndexVec::new(),
            func: func,
            proc: proc,
            name: name.to_string(),
        }
    }

    fn emit_entry_block(&mut self) -> inkwell::basic_block::BasicBlock {
        let block = self.func.append_basic_block("entry");
        self.ctx.builder.position_at_end(&block);

        let null_val = self.ctx.rt.val_type.const_zero();

        for local in self.proc.locals.iter() {
            let name = match local.name {
                Some(ref s) => s,
                None => "",
            };
            let alloc = self.ctx.builder.build_alloca(self.ctx.rt.val_type, name);
            self.ctx.builder.build_store(alloc, null_val);
            self.local_allocs.push(alloc);
        }

        block
    }

    fn finalize_entry_block(&self, entry: &inkwell::basic_block::BasicBlock) {
        self.ctx.builder.position_at_end(&entry);
        self.ctx.builder.build_unconditional_branch(&self.blocks[BlockId::new(0)]);
    }

    fn emit_proc(&mut self, emit: &Emit) {
        let entry_block = self.emit_entry_block();

        for cfg_block in self.proc.blocks.iter() {
            let block = self.func.append_basic_block(&format!("s{}b{}", cfg_block.scope.index(), cfg_block.id.index()));
            self.blocks.push(block);
        }

        for (id, block) in self.proc.blocks.iter().enumerate() { // TODO port iter_enumerated
            let ll_block = &self.blocks[BlockId::new(id)];
            self.ctx.builder.position_at_end(ll_block);
            self.emit_block(block, emit);
        }

        self.finalize_entry_block(&entry_block);
    }

    fn assign_literal(&self, lit: &Literal, local: LocalId) {
        match lit {
            Literal::Num(x) => {
                let lit_val = self.ctx.llvm_ctx.f32_type().const_float(*x as f64);
                let lit_val = self.ctx.builder.build_bitcast(lit_val, self.ctx.llvm_ctx.i32_type(), "lit");

                let disc_val = self.ctx.llvm_ctx.i32_type().const_int(1, false);
                let val = self.ctx.builder.build_load(self.local_allocs[local], "val");
                let val_struct = val.as_struct_value();

                let val_upd = self.ctx.builder.build_insert_value(*val_struct, disc_val, 0, "updval").unwrap();
                let val_upd = self.ctx.builder.build_insert_value(val_upd, lit_val, 1, "updval").unwrap();

                self.ctx.builder.build_store(self.local_allocs[local], val_upd);
                //self.ctx.builder.build_call(self.ctx.rt.rt_val_float, &[self.local_allocs[local].into(), val], "val");
            },
            Literal::Null => {}, // should already be null
            _ => unimplemented!("{:?}", lit),
        }
    }

    fn load_local<L: Borrow<LocalId>>(&self, local: L) -> inkwell::values::BasicValueEnum {
        self.ctx.builder.build_load(self.local_allocs[*local.borrow()], "").into()
    }

    fn emit_block(&self, block: &Block, emit: &Emit) {
        for op in block.ops.iter() {
            match op {
                Op::Literal(id, literal) => {
                    self.assign_literal(literal, *id);
                },

                Op::MkVar(var) => {
                    // nothing now - can we remove this?
                }

                Op::Load(local, var) => {
                    let copy = self.load_local(var);
                    self.ctx.builder.build_store(self.local_allocs[*local], copy);
                    let cloned_val = self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*local].into()], "");
                },

                Op::Store(var, local) => {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[self.local_allocs[*var].into()], "");

                    let copy = self.load_local(local);
                    self.ctx.builder.build_store(self.local_allocs[*var], copy);

                    if self.proc.locals[*local].movable {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_cloned, &[self.local_allocs[*var].into()], "");
                    }
                },

                Op::Put(id) => {
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &[self.local_allocs[*id].into()], "put");
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.local_allocs[*lhs].into();
                    let rhs_ref = self.local_allocs[*rhs].into();
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false).into();

                    self.ctx.builder.build_call(self.ctx.rt.rt_val_binary_op, &[self.local_allocs[*id].into(), op_var, lhs_ref, rhs_ref], "");
                },

                Op::Call(id, name, args) => {
                    assert!(args.len() == 0);
                    let func = emit.sym[name];
                    let res_val = self.ctx.builder.build_call(func, &[], name).try_as_basic_value().left().unwrap();
                    self.ctx.builder.build_store(self.local_allocs[*id], res_val);
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
                self.ctx.builder.build_unconditional_branch(&self.blocks[*id]);
            },

            Terminator::Switch { discriminant, branches, default } => {
                // call runtime to convert value to bool
                let disc_bool = self.ctx.builder.build_call(self.ctx.rt.rt_val_to_switch_disc, &[self.local_allocs[*discriminant].into()], "disc").try_as_basic_value().left().unwrap();

                let disc_int = *disc_bool.as_int_value();

                self.finalize_block(block);

                // convert branches to use llvm blocks
                let llvm_branches = branches.iter().map(|(lit, target)| {
                    let val = self.ctx.llvm_ctx.i32_type().const_int((*lit).into(), false);
                    (val, &self.blocks[*target])
                }).collect::<Vec<_>>();

                self.ctx.builder.build_switch(disc_int, &self.blocks[*default], &llvm_branches[..]);
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
pub struct Emit<'a> {
    ctx: &'a Context,
    procs: Vec<ProcEmit<'a>>,
    sym: HashMap<String, inkwell::values::FunctionValue>
}

impl<'a> Emit<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        Self {
            ctx: ctx,
            procs: Vec::new(),
            sym: HashMap::new(),
        }
    }

    pub fn add_proc(&mut self, name: &str, proc: &'a Proc) {
        let mut proc_emit = ProcEmit::new(self.ctx, proc, name);
        self.sym.insert(name.to_string(), proc_emit.func.clone());
        self.procs.push(proc_emit);
    }

    pub fn emit(&mut self) {
        let mut main_proc = None;
        for mut proc_emit in self.procs.drain(..).collect::<Vec<_>>() { // TODO no
            proc_emit.emit_proc(self);
            if proc_emit.name == "entry" {
                main_proc = Some(proc_emit.func.clone());
            }
        }

        self.emit_main(main_proc.unwrap());
    }

    fn emit_main(&mut self, entry_func: inkwell::values::FunctionValue) {
        let func_type = self.ctx.llvm_ctx.void_type().fn_type(&[], false);
        let func = self.ctx.module.add_function("main", func_type, None);

        let block = func.append_basic_block("entry");
        self.ctx.builder.position_at_end(&block);
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
            let pm = inkwell::passes::PassManager::create_for_module();
            pm.initialize();
            pm_builder.populate_function_pass_manager(&pm);
            pm.run_on_module(&self.ctx.module);
            pm.finalize();

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
pub struct Context {
    llvm_ctx: inkwell::context::Context,
    builder: inkwell::builder::Builder,
    module: inkwell::module::Module,
    rt: RtFuncs,
}

impl Context {
    pub fn new() -> Self {
        let llvm_ctx = inkwell::context::Context::create();
        let module = llvm_ctx.create_module("main");
        let rt = RtFuncs::new(&llvm_ctx, &module);

        Self {
            builder: llvm_ctx.create_builder(),
            module: module,
            llvm_ctx: llvm_ctx,
            rt: rt,
        }
    }
}

macro_rules! rt_funcs {
    ( $name:ident, [ $( ( $func:ident, $ret:ident, [ $( $arg:ident ),* $(,)* ] ) ),* $(,)* ] ) => {
        #[derive(Debug)]
        struct $name {
            // TODO: undo this, it's a hack to clean up optimized output
            val_type: inkwell::types::StructType,
            //val_type: inkwell::types::IntType,
            val_ptr_type: inkwell::types::PointerType,
            $(
                $func: inkwell::values::FunctionValue,
            )*
        }

        impl $name {
            fn new(ctx: &inkwell::context::Context, module: &inkwell::module::Module) -> $name {
                let padding_size = size_of::<ludo::val::Val>() - 4; // u32 discrim
                // TODO: undo this, it's a hack to clean up optimized output
                let val_padding_type = ctx.i32_type();
                let val_type = ctx.struct_type(&[ctx.i32_type().into(), val_padding_type.into()], true);
                assert_eq!(padding_size, 4);
                //let val_type = ctx.i64_type();
                let val_ptr_type = val_type.ptr_type(inkwell::AddressSpace::Generic);

                $name {
                    val_type: val_type.into(),
                    val_ptr_type: val_ptr_type,
                    $(
                        $func: module.add_function(stringify!($func),
                            rt_funcs!(@genty ctx val_ptr_type $ret).fn_type(&[
                                $(
                                    rt_funcs!(@genty ctx val_ptr_type $arg).into(),
                                )*
                            ], false),
                        None),
                    )*
                }
            }
        }
    };

    ( @genty $ctx:ident $ptt:ident val_ptr_type) => (
        $ptt
    );

    ( @genty $ctx:ident $ptt:ident $ret:ident) => (
        $ctx.$ret()
    );
}

rt_funcs!{
    RtFuncs,
    [
        (rt_val_float, void_type, [val_ptr_type, f32_type]),
        (rt_val_int, void_type, [val_ptr_type, i32_type]),
        (rt_val_binary_op, void_type, [val_ptr_type, i32_type, val_ptr_type, val_ptr_type]),
        (rt_val_to_switch_disc, i32_type, [val_ptr_type]),
        (rt_val_print, void_type, [val_ptr_type]),
        (rt_val_cloned, void_type, [val_ptr_type]),
        (rt_val_drop, void_type, [val_ptr_type]),
    ]
}
