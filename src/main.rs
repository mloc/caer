use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default()).unwrap();

    let dm_context = dreammaker::Context::default();
    let preproc = dreammaker::preprocessor::Preprocessor::new(&dm_context, "main.dm".into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();
    let main_proc = &tree.root().get().procs["main"].value[0];

    let proc = build_statements(&main_proc.body);
    println!("{:?}", proc);

    let builder = Builder::new();
    builder.emit_proc(&proc);

    println!("Hello, world!");
}

fn build_statements(root: &Vec<ast::Statement>) -> Proc {
    let mut proc = Proc::new();

    // finddecls
    for stmt in root.iter() {
        if let ast::Statement::Var(v) = stmt {
            proc.add_local(Some(&v.name));
        }
    }

    // emit, we have a single block
    let block = proc.build_block(&root);
    proc.blocks.push(block);

    proc
}

impl Proc {
    fn new() -> Self {
        let mut new = Self {
            locals: IndexVec::new(),
            vars: HashMap::new(),
            blocks: Vec::new(),
        };
        new.add_local(None); // return
        new
    }

    fn add_local(&mut self, name: Option<&str>) -> LocalId {
        let id = LocalId::new(self.locals.len());

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
                ast::Statement::Var(v) => {
                    if let Some(ref expr) = v.value {
                        self.build_assign(&v.name, expr, &mut block);
                    }
                },

                ast::Statement::Expr(expr) => {
                    match expr {
                        ast::Expression::AssignOp { op, lhs, rhs } => {
                            let var = match lhs.as_ref() {
                                ast::Expression::Base { unary, term, follow } => {
                                    match term {
                                        ast::Term::Ident(ref s) => s,
                                        _ => unimplemented!(),
                                    }
                                },

                                _ => unimplemented!(),
                            };

                            self.build_assign(var, &*rhs, &mut block);
                        },

                        ast::Expression::BinaryOp { op, lhs, rhs } => {
                            let local = self.add_local(None);
                            let res = self.build_expr(rhs, &mut block);
                            block.ops.push(Op::Mov(local, res));
                            match op {
                                ast::BinaryOp::LShift => {
                                    block.ops.push(Op::Put(local));
                                },
                                _ => unimplemented!(),
                            };
                        },

                        _ => unimplemented!(),
                    }
                },

                _ => unimplemented!(),
            }
        }

        block
    }

    fn build_assign(&mut self, var: &str, expr: &ast::Expression, block: &mut Block) {
        let var_id = self.lookup_var(var).unwrap();
        let asg_expr = self.build_expr(expr, block);
        block.ops.push(Op::Mov(var_id, asg_expr));
    }

    fn build_expr(&mut self, expr: &ast::Expression, block: &mut Block) -> Expr {
        match expr {
            ast::Expression::Base { unary, term, follow } => {
                assert!(unary.len() == 0);
                assert!(follow.len() == 0);
                self.build_term(term)
            },

            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs_expr = self.build_expr(lhs, block);
                let rhs_expr = self.build_expr(rhs, block);
                let local = self.add_local(None);

                match op {
                    ast::BinaryOp::Add => {
                        block.ops.push(Op::Add(local, lhs_expr, rhs_expr));
                    },

                    _ => unimplemented!(),
                }

                Expr::Place(Place::Local(local))
            },

            _ => unimplemented!(),
        }
    }

    fn build_term(&self, term: &ast::Term) -> Expr {
        match term {
            ast::Term::Int(x) => Expr::Literal(Literal::Num(*x as f32)),
            ast::Term::Float(x) => Expr::Literal(Literal::Num(*x)),
            ast::Term::Ident(var_name) => {
                let var_id = self.lookup_var(var_name).unwrap();
                Expr::Place(Place::Local(var_id))
            },
            _ => unimplemented!(),
        }
    }

    fn lookup_var(&self, var: &str) -> Option<LocalId> {
        self.vars.get(var).map(|id| *id)
    }
}

newtype_index!(LocalId);

#[derive(Debug)]
struct Local {
    id: LocalId,
    name: Option<String>,
}

#[derive(Debug)]
struct Proc {
    locals: IndexVec<LocalId, Local>,
    vars: HashMap<String, LocalId>,

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
    Mov(LocalId, Expr),
    Put(LocalId),
    Add(LocalId, Expr, Expr),
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
    Local(LocalId),
    Global(u32),
}

#[derive(Debug)]
enum Terminator {
    Return,
    Jump(u32),
    Switch {
        discriminant: Place,
        branches: Vec<(Literal, u32)>,
        default: Option<u32>,
    },
}

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
                    let print_type = self.ctx.void_type().fn_type(&[self.ctx.f32_type().into()], false);
                    let print_func = self.module.add_function("dm_print_num", print_type, None);
                    let val = self.builder.build_load(proc_emit.local_allocs[*id], "put");
                    self.builder.build_call(print_func, &mut [val], "put");
                },
                Op::Add(id, lhs, rhs) => {
                    let lhs_ref = self.load_expr(lhs, proc_emit).into_float_value();
                    let rhs_ref = self.load_expr(rhs, proc_emit).into_float_value();
                    let val = self.builder.build_float_add(lhs_ref, rhs_ref, "sum");
                    self.builder.build_store(proc_emit.local_allocs[*id], val);
                }
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
