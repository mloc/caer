use indexed_vec::{IndexVec, Idx};
use crate::cfg::*;

#[derive(Debug)]
struct ProcEmit {
    local_allocs: IndexVec<LocalId, inkwell::values::PointerValue>,
    blocks: Vec<inkwell::basic_block::BasicBlock>,
    func: inkwell::values::FunctionValue,
}

impl ProcEmit {
    fn new(func: inkwell::values::FunctionValue) -> Self {
        Self {
            local_allocs: IndexVec::new(),
            blocks: Vec::new(),
            func: func,
        }
    }
}

#[derive(Debug)]
pub struct Builder {
    ctx: inkwell::context::Context,
    builder: inkwell::builder::Builder,
    module: inkwell::module::Module,
    rt: RtFuncs,
}

impl Builder {
    pub fn new() -> Self {
        let ctx = inkwell::context::Context::create();
        let module = ctx.create_module("main");
        let rt = RtFuncs::new(&ctx, &module);
        Self {
            builder: ctx.create_builder(),
            module: module,
            ctx: ctx,
            rt: rt,
        }
    }

    fn emit_entry_block(&self, proc: &Proc, proc_emit: &mut ProcEmit) -> inkwell::basic_block::BasicBlock {
        let block = proc_emit.func.append_basic_block("entry");
        self.builder.position_at_end(&block);

        for local in proc.locals.iter() {
            let name = match local.name {
                Some(ref s) => s,
                None => "",
            };
            let alloc = self.builder.build_alloca(self.rt.ptr_type, name);
            proc_emit.local_allocs.push(alloc);
        }

        block
    }

    fn finalize_entry_block(&self, entry: &inkwell::basic_block::BasicBlock, start: &inkwell::basic_block::BasicBlock) {
        self.builder.position_at_end(&entry);
        self.builder.build_unconditional_branch(&start);
    }

    pub fn emit_proc(&self, proc: &Proc) {
        let func_type = self.ctx.f32_type().fn_type(&[], false);
        let func = self.module.add_function("main", func_type, None);
        let mut proc_emit = ProcEmit::new(func);
        let entry_block = self.emit_entry_block(proc, &mut proc_emit);

        for block_id in 0..proc.blocks.len() {
            let block = proc_emit.func.append_basic_block(&format!("f{}", block_id));
            proc_emit.blocks.push(block);
        }

        for (id, block) in proc.blocks.iter().enumerate() {
            let ll_block = &proc_emit.blocks[id];
            self.builder.position_at_end(ll_block);
            self.emit_block(block, &proc_emit);
        }

        self.finalize_entry_block(&entry_block, &proc_emit.blocks[0]);

        self.module.print_to_stderr();

        let engine = self.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

        unsafe {
            let func = engine.get_function::<unsafe extern "C" fn()>("main").unwrap();
            func.call();
        }
    }

    fn lit_to_val(&self, lit: &Literal) -> inkwell::values::BasicValueEnum {
        let val = match lit {
            Literal::Num(x) => self.ctx.f32_type().const_float(*x as f64).into(),
            _ => self.ctx.f32_type().const_float(0f64).into(),
        };

        self.builder.build_call(self.rt.rt_val_float, &[val], "val").try_as_basic_value().left().unwrap()
    }

    fn load_expr(&self, expr: &Expr, proc_emit: &ProcEmit) -> inkwell::values::BasicValueEnum {
        match expr {
            Expr::Literal(lit) => self.lit_to_val(lit).into(),
            Expr::Place(place) => {
                match place {
                    Place::Local(id) => {
                        self.builder.build_load(proc_emit.local_allocs[*id], "").into()
                    },
                    Place::Global(_) => panic!("todo"),
                }
            }
        }
    }

    fn emit_block(&self, block: &Block, proc_emit: &ProcEmit) {
        for op in block.ops.iter() {
            match op {
                Op::Mov(id, expr) => {
                    let val = self.load_expr(expr, proc_emit);
                    self.builder.build_store(proc_emit.local_allocs[*id], val);
                },

                Op::Put(id) => {
                    let val = self.builder.build_load(proc_emit.local_allocs[*id], "put");
                    self.builder.build_call(self.rt.rt_val_print, &mut [val], "put");
                },
                Op::Add(id, lhs, rhs) => {
                    let lhs_ref = self.load_expr(lhs, proc_emit);
                    let rhs_ref = self.load_expr(rhs, proc_emit);
                    let res_val = self.builder.build_call(self.rt.rt_val_add, &[lhs_ref, rhs_ref], "sum").try_as_basic_value().left().unwrap();
                    self.builder.build_store(proc_emit.local_allocs[*id], res_val);
                }
                _ => unimplemented!("{:?}", op),
            }
        }

        match &block.terminator {
            Terminator::Return => {
                self.builder.build_return(Some(&proc_emit.local_allocs[LocalId::new(0)]));
            },

            Terminator::Jump(id) => {
                self.builder.build_unconditional_branch(&proc_emit.blocks[*id as usize]);
            },

            Terminator::Switch { discriminant, branches, default } => {
                unimplemented!();
            },
        }
    }
}

#[no_mangle]
pub extern "C" fn dm_print_num(num: f32) {
    println!("put: {}", num);
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
