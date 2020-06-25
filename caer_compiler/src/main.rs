mod emit;
mod frontend;
mod ir;
mod llvm;
mod ty;

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let dm_context = dreammaker::Context::default();
    let preproc =
        dreammaker::preprocessor::Preprocessor::new(&dm_context, "main.dm".into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();
    println!("PARSED");

    let mut env = ir::env::Env::new();
    let mut tb = frontend::TreeBuilder::new(&tree, &mut env);
    tb.build();
    println!("CFG BUILT");

    let llctx = inkwell::context::Context::create();
    let llmod = llctx.create_module("main");
    let llbuild = llctx.create_builder();

    let emit_ctx = emit::Context::new(&llctx, &llmod, &llbuild);
    let mut builder = emit::ProgEmit::new(&emit_ctx, &env);
    builder.build_procs();
    println!("EMIT DONE");

    builder.emit();
    builder.run(true);
}
