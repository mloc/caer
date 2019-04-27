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

    let frontend = frontend::Builder::new(&tree);
    let procs = frontend.build_procs();

    let emit_ctx = emit::Context::new();
    let mut builder = emit::Emit::new(&emit_ctx);
    for (name, proc) in procs.iter() {
        builder.add_proc(&name, &proc);
    }

    builder.emit();
    builder.run(true);
}
