mod cfg;
mod emit;
mod frontend;
mod ty;

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default()).unwrap();

    let dm_context = dreammaker::Context::default();
    let preproc = dreammaker::preprocessor::Preprocessor::new(&dm_context, "main.dm".into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();
    println!("PARSED");

    let env = frontend::Builder::build(&tree);
    println!("CFG BUILT");

    let llctx = inkwell::context::Context::create();
    let llmod = llctx.create_module("main");
    let llbuild = llctx.create_builder();

    let emit_ctx = emit::Context::new(&llctx, &llmod, &llbuild);
    let mut builder = emit::Emit::new(&emit_ctx, &env);
    builder.build_procs();
    println!("EMIT DONE");

    builder.emit();
    builder.run(true);
}
