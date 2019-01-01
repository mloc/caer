mod cfg;
mod emit;
mod frontend;

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default()).unwrap();

    let dm_context = dreammaker::Context::default();
    let preproc = dreammaker::preprocessor::Preprocessor::new(&dm_context, "main.dm".into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();
    let main_proc = &tree.root().get().procs["main"].value[0];

    let frontend = frontend::Builder::new(&tree);
    let procs = frontend.build_procs();

    let builder = emit::Builder::new();
    for (name, proc) in procs.iter() {
        builder.emit_proc(&name, &proc);
    }

    builder.run();
}
