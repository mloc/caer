extern crate lalrpop_util;
extern crate inkwell;

use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub basic);

mod ast;

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default()).unwrap();

    let mut file = File::open("exp.bsc").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();

    let parser = basic::StatementsParser::new();

    let res = parser.parse(&buf);
    println!("{:?}", res);
    let proc = build_statements(res.unwrap());
    println!("{:?}", proc);

    let builder = Builder::new();
    builder.emit_proc(&proc);

    println!("Hello, world!");
}

fn build_statements(root: Vec<ast::Statement>) -> Proc {
    let mut proc = Proc::new();

    // finddecls
    for stmt in root.iter() {
        if let ast::Statement::Decl(v) = stmt {
            proc.add_local(Some(&v));
        }
    }

    // emit, we have a single block
    let block = proc.build_block(&root);
    proc.blocks.push(block);

    proc
}

impl Proc {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            vars: HashMap::new(),
            blocks: Vec::new(),
        }
    }

    fn add_local(&mut self, name: Option<&str>) -> u32 {
        let id = self.locals.len() as u32;

        let local = Local {
            id: id,
            name: name.map(|s| s.to_string()),
        };

        self.locals.push(local);

        if let Some(var) = name {
            self.vars.insert(var.into(), id);
        }

        id
    }

    fn build_block(&mut self, stmts: &[ast::Statement]) -> Block {
        let mut block = Block::new();

        for stmt in stmts.iter() {
            match stmt {
                ast::Statement::Decl(_) => {}, // unused for now, scoping

                ast::Statement::Assignment(var, expr) => {
                    let var_id = self.lookup_var(var).unwrap();
                    let asg_expr = self.build_expr(expr, &mut block);
                    block.ops.push(Op::Mov(var_id, asg_expr));
                },

                ast::Statement::Put(var) => {
                    let var_id = self.lookup_var(var).unwrap();
                    block.ops.push(Op::Put(var_id));
                },
            }
        }

        block
    }

    fn build_expr(&mut self, expr: &ast::Expr, block: &mut Block) -> Expr {
        match expr {
            ast::Expr::Term(t) => self.build_term(t),
            ast::Expr::Add(lhs, rhs) => {
                let lhs_expr = self.build_expr(lhs, block);
                let rhs_expr = self.build_expr(rhs, block);
                let local = self.add_local(None);
                block.ops.push(Op::Add(local, lhs_expr, rhs_expr));
                Expr::Place(Place::Local(local))
            }
        }
    }

    fn build_term(&self, term: &ast::Term) -> Expr {
        match term {
            ast::Term::Literal(x) => Expr::Literal(Literal::Num(*x as f32)),
            ast::Term::Ref(var_name) => {
                let var_id = self.lookup_var(var_name).unwrap();
                Expr::Place(Place::Local(var_id))
            },
        }
    }

    fn lookup_var(&self, var: &str) -> Option<u32> {
        self.vars.get(var).map(|id| *id)
    }
}

type LocalID = u32;

#[derive(Debug)]
struct Local {
    id: u32,
    name: Option<String>,
}

#[derive(Debug)]
struct Proc {
    locals: Vec<Local>,
    vars: HashMap<String, u32>,

    blocks: Vec<Block>,
}

impl Proc {
}

#[derive(Debug)]
struct Block {
    ops: Vec<Op>,
    terminator: Terminator,
}

impl Block {
    fn new() -> Self {
        Self {
            ops: Vec::new(),
            terminator: Terminator::Return,
        }
    }
}

#[derive(Debug)]
enum Op {
    Mov(u32, Expr),
    Put(u32),
    Add(u32, Expr, Expr),
}

#[derive(Debug)]
enum Literal {
    Null,
    Num(f32),
    String(String),
    List,
}

#[derive(Debug)]
enum Expr {
    Literal(Literal),
    Place(Place),
}

#[derive(Debug)]
enum Place {
    Local(u32),
    Global(u32),
}

#[derive(Debug)]
enum Terminator {
    Return,
    Switch {
        discriminant: Place,
        branches: Vec<(Literal, u32)>,
        default: Option<u32>,
    },
}

#[derive(Debug)]
struct ProcEmit {
    local_allocs: Vec<inkwell::values::PointerValue>,
    blocks: Vec<inkwell::basic_block::BasicBlock>,
    func: inkwell::values::FunctionValue,
}

impl ProcEmit {
    fn new(func: inkwell::values::FunctionValue) -> Self {
        Self {
            local_allocs: Vec::new(),
            blocks: Vec::new(),
            func: func,
        }
    }
}

#[derive(Debug)]
struct Builder {
    ctx: inkwell::context::Context,
    builder: inkwell::builder::Builder,
    module: inkwell::module::Module,
}

impl Builder {
    fn new() -> Self {
        let ctx = inkwell::context::Context::create();
        Self {
            builder: ctx.create_builder(),
            module: ctx.create_module("main"),
            ctx: ctx,
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
            let alloc = self.builder.build_alloca(self.ctx.f32_type(), name);
            proc_emit.local_allocs.push(alloc);
        }

        block
    }

    fn finalize_entry_block(&self, entry: &inkwell::basic_block::BasicBlock, start: &inkwell::basic_block::BasicBlock) {
        self.builder.position_at_end(&entry);
        self.builder.build_unconditional_branch(&start);
    }

    fn emit_proc(&self, proc: &Proc) {
        let func_type = self.ctx.void_type().fn_type(&[], false);
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
        match lit {
            Literal::Num(x) => self.ctx.f32_type().const_float(*x as f64).into(),
            _ => self.ctx.f32_type().const_float(0f64).into(),
        }
    }

    fn load_expr(&self, expr: &Expr, proc_emit: &ProcEmit) -> inkwell::values::BasicValueEnum {
        match expr {
            Expr::Literal(lit) => self.lit_to_val(lit).into(),
            Expr::Place(place) => {
                match place {
                    Place::Local(id) => {
                        self.builder.build_load(proc_emit.local_allocs[*id as usize], "").into()
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
                    self.builder.build_store(proc_emit.local_allocs[*id as usize], val);
                },

                Op::Put(id) => {
                    let print_type = self.ctx.void_type().fn_type(&[self.ctx.f32_type().into()], false);
                    //let print_func = self.module.get_or_add_function("dm_print_num", print_type);
                    let print_func = self.module.add_function("dm_print_num", print_type, None);
                    let val = self.builder.build_load(proc_emit.local_allocs[*id as usize], "put");
                    self.builder.build_call(print_func, &mut [val], "put");
                },
                Op::Add(id, lhs, rhs) => {
                    let lhs_ref = self.load_expr(lhs, proc_emit).into_float_value();
                    let rhs_ref = self.load_expr(rhs, proc_emit).into_float_value();
                    let val = self.builder.build_float_add(lhs_ref, rhs_ref, "sum");
                    self.builder.build_store(proc_emit.local_allocs[*id as usize], val);
                }
            }
        }

        self.builder.build_return(None);
    }
}

#[no_mangle]
pub extern "C" fn dm_print_num(num: f32) {
    println!("put: {}", num);
}
