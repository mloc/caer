use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use dreammaker::ast;
use indexed_vec::{IndexVec, newtype_index, Idx};

mod cfg;
use crate::cfg::*;

mod emit;

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

    let builder = emit::Builder::new();
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
