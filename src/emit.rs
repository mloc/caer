use indexed_vec::{IndexVec, Idx};
use crate::cfg::*;

#[derive(Debug)]
struct ProcEmit<'a> {
    ctx: &'a Context,
    local_allocs: IndexVec<LocalId, inkwell::values::PointerValue>,
    blocks: Vec<inkwell::basic_block::BasicBlock>,
    func: inkwell::values::FunctionValue,
    proc: &'a Proc,
}

impl<'a> ProcEmit<'a> {
    fn new(ctx: &'a Context, proc: &'a Proc, name: &str) -> Self {
        let func_type = ctx.llvm_ctx.f32_type().fn_type(&[], false);
        let func = ctx.module.add_function(name, func_type, None);

        Self {
            ctx: ctx,
            local_allocs: IndexVec::new(),
            blocks: Vec::new(),
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

        block
    }

    fn finalize_entry_block(&self, entry: &inkwell::basic_block::BasicBlock) {
        self.ctx.builder.position_at_end(&entry);
        self.ctx.builder.build_unconditional_branch(&self.blocks[0]);
    }

    fn emit_proc(&mut self) {
        let entry_block = self.emit_entry_block();

        for block_id in 0..self.proc.blocks.len() {
            let block = self.func.append_basic_block(&format!("f{}", block_id));
            self.blocks.push(block);
        }

        for (id, block) in self.proc.blocks.iter().enumerate() {
            let ll_block = &self.blocks[id];
            self.ctx.builder.position_at_end(ll_block);
            self.emit_block(block);
        }

        self.finalize_entry_block(&entry_block);
    }

    fn lit_to_val(&self, lit: &Literal) -> inkwell::values::BasicValueEnum {
        let val = match lit {
            Literal::Num(x) => self.ctx.llvm_ctx.f32_type().const_float(*x as f64).into(),
            _ => self.ctx.llvm_ctx.f32_type().const_float(0f64).into(),
        };

        self.ctx.builder.build_call(self.ctx.rt.rt_val_float, &[val], "val").try_as_basic_value().left().unwrap()
    }

    fn load_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum {
        match expr {
            Expr::Literal(lit) => self.lit_to_val(lit).into(),
            Expr::Place(place) => {
                match place {
                    Place::Local(id) => {
                        self.ctx.builder.build_load(self.local_allocs[*id], "").into()
                    },
                    Place::Global(_) => panic!("todo"),
                }
            }
        }
    }

    fn emit_block(&self, block: &Block) {
        for op in block.ops.iter() {
            match op {
                Op::Mov(id, expr) => {
                    let val = self.load_expr(expr);
                    self.ctx.builder.build_store(self.local_allocs[*id], val);
                },

                Op::Put(id) => {
                    let val = self.ctx.builder.build_load(self.local_allocs[*id], "put");
                    self.ctx.builder.build_call(self.ctx.rt.rt_val_print, &mut [val], "put");
                },
                Op::Add(id, lhs, rhs) => {
                    let lhs_ref = self.load_expr(lhs);
                    let rhs_ref = self.load_expr(rhs);
                    let res_val = self.ctx.builder.build_call(self.ctx.rt.rt_val_add, &[lhs_ref, rhs_ref], "sum").try_as_basic_value().left().unwrap();
                    self.ctx.builder.build_store(self.local_allocs[*id], res_val);
                }
                _ => unimplemented!("{:?}", op),
            }
        }

        match &block.terminator {
            Terminator::Return => {
                self.ctx.builder.build_return(Some(&self.local_allocs[LocalId::new(0)]));
            },

            Terminator::Jump(id) => {
                self.ctx.builder.build_unconditional_branch(&self.blocks[*id as usize]);
            },

            Terminator::Switch { discriminant, branches, default } => {
                unimplemented!();
            },
        }
    }
}

#[derive(Debug)]
pub struct Emit<'a> {
    ctx: &'a Context,
    procs: Vec<ProcEmit<'a>>,
}

impl<'a> Emit<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        Self {
            ctx: ctx,
            procs: Vec::new(),
        }
    }

    pub fn add_proc(&mut self, name: &str, proc: &'a Proc) {
        let mut proc_emit = ProcEmit::new(self.ctx, proc, name);
        self.procs.push(proc_emit);
    }

    pub fn emit(&mut self) {
        for proc_emit in self.procs.iter_mut() {
            proc_emit.emit_proc();
        }
    }

    pub fn run(&self) {
        self.ctx.module.print_to_stderr();

        let engine = self.ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

        unsafe {
            let func = engine.get_function::<unsafe extern "C" fn()>("main").unwrap();
            func.call();
        }
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
        (rt_val_add, ptr_type, [ptr_type, ptr_type]),
        (rt_val_print, void_type, [ptr_type]),
        (rt_val_drop, void_type, [ptr_type]),
    ]
}
