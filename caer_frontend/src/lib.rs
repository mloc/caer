mod block_builder;
mod func_builder;
mod ir_builder;
mod proc_builder;
mod tree;

use std::path::Path;

// temporary
pub fn build(path: impl AsRef<Path>) -> caer_ir::env::Env {
    let dm_context = dreammaker::Context::default();
    let preproc =
        dreammaker::preprocessor::Preprocessor::new(&dm_context, path.as_ref().into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();

    let mut env = caer_ir::env::Env::new();
    let mut tb = tree::TreeBuilder::new(&tree, &mut env);
    tb.build();
    env
}
