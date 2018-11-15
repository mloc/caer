extern crate dreammaker;
extern crate llvm_sys_wrapper;
extern crate lalrpop_util;

use llvm_sys_wrapper as llvm;
use llvm_sys_wrapper::LLVMFunctionType; // ugh
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub basic);

mod ast;

fn main() {
    llvm::LLVM::initialize();

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

    let mut local_id = 0u32;

    // finddecls
    for stmt in root.iter() {
        if let ast::Statement::Decl(v) = stmt {
            proc.locals.insert(v.clone(), local_id);
            proc.local_defs.push(local_id);
            local_id += 1;
        }
    }

    // emit, we have a single block
    let block = proc.build_block(&root);
    proc.blocks.push(block);

    proc
}

impl Proc {
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
                let res_id = self.local_defs.len() as u32;
                self.local_defs.push(res_id);
                block.ops.push(Op::Add(res_id, lhs_expr, rhs_expr));
                Expr::Place(Place::Local(res_id))
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
        self.locals.get(var).map(|id| *id)
    }
}

type LocalID = u32;

#[derive(Debug)]
struct Proc {
    locals: HashMap<String, u32>,

    local_defs: Vec<u32>,

    blocks: Vec<Block>,
}

impl Proc {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            local_defs: Vec::new(),
            blocks: Vec::new(),
        }
    }
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
    local_allocs: Vec<llvm::LLVMValueRef>,
    blocks: Vec<llvm::LLVMBasicBlockRef>,
    func: llvm::Function,
}

impl ProcEmit {
    fn new(func: llvm::Function) -> Self {
        Self {
            local_allocs: Vec::new(),
            blocks: Vec::new(),
            func: func,
        }
    }
}

#[derive(Debug)]
struct Builder {
    ctx: llvm::Context,
    builder: llvm::Builder,
    module: llvm::Module,
}

impl Builder {
    fn new() -> Self {
        let ctx = llvm::Context::global_context();
        Self {
            builder: ctx.create_builder(),
            module: ctx.create_module("main"),
            ctx: ctx,
        }
    }

    fn emit_entry_block(&self, proc: &Proc, proc_emit: &mut ProcEmit) -> llvm::LLVMBasicBlockRef {
        let block = proc_emit.func.append_basic_block("entry");
        self.builder.position_at_end(block);

        for id in proc.local_defs.iter() {
            let alloc = self.builder.build_alloca(self.ctx.FloatType());
            proc_emit.local_allocs.push(alloc);
        }

        block
    }

    fn finalize_entry_block(&self, entry: llvm::LLVMBasicBlockRef, start: llvm::LLVMBasicBlockRef) {
        self.builder.position_at_end(entry);
        self.builder.build_br(start);
    }

    fn emit_proc(&self, proc: &Proc) {
        let func_type = llvm::fn_type!(self.ctx.VoidType());
        let func = self.module.add_function("main", func_type);
        let mut proc_emit = ProcEmit::new(func);
        let entry_block = self.emit_entry_block(proc, &mut proc_emit);

        for block_id in 0..proc.blocks.len() {
            let block = proc_emit.func.append_basic_block(&format!("f{}", block_id));
            proc_emit.blocks.push(block);
        }

        for (id, block) in proc.blocks.iter().enumerate() {
            let ll_block = proc_emit.blocks[id];
            self.builder.position_at_end(ll_block);
            self.emit_block(block, &proc_emit);
        }

        self.finalize_entry_block(entry_block, proc_emit.blocks[0]);

        self.module.dump();

        let engine = llvm::Engine::create_jit_engine(self.module.as_ref()).unwrap();;
        engine.run_function(proc_emit.func.as_ref(), &mut []);
    }

    fn lit_to_val(&self, lit: &Literal) -> llvm::LLVMValueRef {
        match lit {
            Literal::Num(x) => self.ctx.Float(*x as f64),
            _ => self.ctx.Float(0f64),
        }
    }

    fn load_expr(&self, expr: &Expr, proc_emit: &ProcEmit) -> llvm::LLVMValueRef {
        match expr {
            Expr::Literal(lit) => self.lit_to_val(lit),
            Expr::Place(place) => {
                match place {
                    Place::Local(id) => {
                        self.builder.build_load(proc_emit.local_allocs[*id as usize])
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
                    self.builder.build_store(val, proc_emit.local_allocs[*id as usize]);
                },

                Op::Put(id) => {
                    let print_type = llvm::fn_type!(self.ctx.VoidType(), self.ctx.FloatType());
                    let print_func = self.module.get_or_add_function("dm_print_num", print_type);
                    let val = self.builder.build_load(proc_emit.local_allocs[*id as usize]);
                    self.builder.build_call(print_func.as_ref(), &mut [val]);
                },
                Op::Add(id, lhs, rhs) => {
                    let lhs_ref = self.load_expr(lhs, proc_emit);
                    let rhs_ref = self.load_expr(rhs, proc_emit);
                    let val = self.builder.build_fadd(lhs_ref, rhs_ref);
                    self.builder.build_store(val, proc_emit.local_allocs[*id as usize]);
                }
            }
        }

        self.builder.build_ret_void();
    }
}

#[no_mangle]
pub extern "C" fn dm_print_num(num: f32) {
    println!("put: {}", num);
}
