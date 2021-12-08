mod block_builder;
mod func_builder;
mod ir_builder;
mod objtree_wrapper;
mod proc_builder;
mod tree_builder;

use std::path::Path;

use caer_ir::module::Module;
use caer_ir::string::StringTable;
use ir_builder::IrBuilder;

// temporary
pub fn build(path: impl AsRef<Path>) -> caer_ir::module::Module {
    let dm_context = dreammaker::Context::default();
    let preproc =
        dreammaker::preprocessor::Preprocessor::new(&dm_context, path.as_ref().into()).unwrap();
    let indents = dreammaker::indents::IndentProcessor::new(&dm_context, preproc);
    let mut parser = dreammaker::parser::Parser::new(&dm_context, indents);
    parser.enable_procs();

    let tree = parser.parse_object_tree();

    let mut strings = StringTable::new();
    let bundle = tree_builder::TreeBuilder::new(&tree, &mut strings).build();

    let mut module = Module::new(strings, bundle.type_tree, bundle.instances);

    let irb = IrBuilder::new(&mut module, bundle.funcs, &bundle.objtree);
    irb.build();

    module
}
