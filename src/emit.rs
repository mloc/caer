use indexed_vec::{IndexVec, Idx};
use std::collections::HashMap;
use crate::cfg::*;
use std::fs;
use std::borrow::Borrow;

#[derive(Debug)]
struct ProcEmit<'a> {
    ctx: &'a Context,
    local_allocs: IndexVec<LocalId, inkwell::values::PointerValue>,
    blocks: IndexVec<BlockId, inkwell::basic_block::BasicBlock>,
    func: inkwell::values::FunctionValue,
    proc: &'a Proc,
}

impl<'a> ProcEmit<'a> {
    fn new(ctx: &'a Context, proc: &'a Proc, name: &str) -> Self {
        let func_type = ctx.rt.ptr_type.fn_type(&[], false);
        let func = ctx.module.add_function(name, func_type, None);

        Self {
            ctx: ctx,
            local_allocs: IndexVec::new(),
            blocks: IndexVec::new(),
            func: func,
            proc: proc,
        }
    }

    fn emit_entry_block(&mut self) -> inkwell::basic_block::BasicBlock {
        let block = self.func.append_basic_block("entry");
        self.ctx.builder.position_at_end(&block);

        for local in self.proc.locals.iter() {
            let name = match local.name {
                Some(ref s) => s,
                None => "",
            };
            let alloc = self.ctx.builder.build_alloca(self.ctx.rt.ptr_type, name);
            self.local_allocs.push(alloc);
        }

        // TODO eww fix me
        let null_init = self.ctx.builder.build_call(self.ctx.rt.rt_val_null, &[], "val").try_as_basic_value().left().unwrap();
        self.ctx.builder.build_store(self.local_allocs[LocalId::new(0)], null_init);

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

    fn lit_to_val(&self, lit: &Literal) -> inkwell::values::BasicValueEnum {
        match lit {
            Literal::Num(x) => {
                let val = self.ctx.llvm_ctx.f32_type().const_float(*x as f64).into();
                self.ctx.builder.build_call(self.ctx.rt.rt_val_float, &[val], "val").try_as_basic_value().left().unwrap()
            },
            Literal::Null => {
                self.ctx.builder.build_call(self.ctx.rt.rt_val_null, &[], "val").try_as_basic_value().left().unwrap()
            }
            _ => unimplemented!("{:?}", lit),
        }
    }

    fn load_local<L: Borrow<LocalId>>(&self, local: L) -> inkwell::values::BasicValueEnum {
        self.ctx.builder.build_load(self.local_allocs[*local.borrow()], "").into()
    }

    fn emit_block(&self, block: &Block, emit: &Emit) {
        for op in block.ops.iter() {
            match op {
                Op::Mov(id, local) => {
                    let val = self.load_local(local);
                    let cloned_val = if self.proc.locals[*local].movable {
                        val
                    } else {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_clone, &[val], "clone").try_as_basic_value().left().unwrap()
                    };
                    self.ctx.builder.build_store(self.local_allocs[*id], cloned_val);
                },

                Op::Literal(id, literal) => {
                    let val = self.lit_to_val(literal);
                    self.ctx.builder.build_store(self.local_allocs[*id], val);
                },

                Op::MkVar(var) => {
                    let null_init = self.ctx.builder.build_call(self.ctx.rt.rt_val_null, &[], "val").try_as_basic_value().left().unwrap();
                    self.ctx.builder.build_store(self.local_allocs[*var], null_init);
                }

                Op::Load(local, var) => {
                    let val = self.load_local(var);
                    let cloned_val = self.ctx.builder.build_call(self.ctx.rt.rt_val_clone, &[val], "clone").try_as_basic_value().left().unwrap();
                    self.ctx.builder.build_store(self.local_allocs[*local], cloned_val);
                },

                Op::Store(var, local) => {
                    let old_val = self.load_local(var);
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[old_val], "");

                    let new_val = self.load_local(local);
                    let cloned_val = if self.proc.locals[*local].movable {
                        new_val
                    } else {
                        self.ctx.builder.build_call(self.ctx.rt.rt_val_clone, &[new_val], "clone").try_as_basic_value().left().unwrap()
                    };
                    self.ctx.builder.build_store(self.local_allocs[*var], cloned_val);
                },

                Op::Put(id) => {
                    let val = self.ctx.builder.build_load(self.local_allocs[*id], "put");
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &mut [val], "put");
                },

                Op::Binary(id, op, lhs, rhs) => {
                    let lhs_ref = self.load_local(lhs);
                    let rhs_ref = self.load_local(rhs);
                    let op_var = self.ctx.llvm_ctx.i32_type().const_int(*op as u64, false).into();

                    let res_val = self.ctx.builder.build_call(self.ctx.rt.rt_val_binary_op, &[op_var, lhs_ref, rhs_ref], "sum").try_as_basic_value().left().unwrap();
                    self.ctx.builder.build_store(self.local_allocs[*id], res_val);
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
                let disc_val = self.load_local(discriminant);

                // call runtime to convert value to bool
                let disc_bool = self.ctx.builder.build_call(self.ctx.rt.rt_val_to_switch_disc, &[disc_val], "disc").try_as_basic_value().left().unwrap();

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
            for local in self.proc.scopes[block.scope].destruct_locals.iter() {
                let local_val = self.load_local(local);
                self.ctx.builder.build_call(self.ctx.rt.rt_val_drop, &[local_val], "");
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
        for mut proc_emit in self.procs.drain(..).collect::<Vec<_>>() { // TODO no
            proc_emit.emit_proc(self);
        }
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
            ptr_type: inkwell::types::PointerType,
            $(
                $func: inkwell::values::FunctionValue,
            )*
        }

        impl $name {
            fn new(ctx: &inkwell::context::Context, module: &inkwell::module::Module) -> $name {
                let ptr_type = ctx.opaque_struct_type("opaque").ptr_type(inkwell::AddressSpace::Generic);

                $name {
                    ptr_type: ptr_type,
                    $(
                        $func: module.add_function(stringify!($func),
                            rt_funcs!(@genty ctx ptr_type $ret).fn_type(&[
                                $(
                                    rt_funcs!(@genty ctx ptr_type $arg).into(),
                                )*
                            ], false),
                        None),
                    )*
                }
            }
        }
    };

    ( @genty $ctx:ident $ptt:ident ptr_type) => (
        $ptt
    );

    ( @genty $ctx:ident $ptt:ident $ret:ident) => (
        $ctx.$ret()
    );
}

rt_funcs!{
    RtFuncs,
    [
        (rt_val_float, ptr_type, [f32_type]),
        (rt_val_int, ptr_type, [i32_type]),
        (rt_val_null, ptr_type, []),
        (rt_val_binary_op, ptr_type, [i32_type, ptr_type, ptr_type]),
        (rt_val_to_switch_disc, i32_type, [ptr_type]),
        (rt_val_print, void_type, [ptr_type]),
        (rt_val_clone, ptr_type, [ptr_type]),
        (rt_val_drop, void_type, [ptr_type]),
    ]
}
