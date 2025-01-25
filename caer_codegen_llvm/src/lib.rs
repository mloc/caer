use std::rc::Rc;

mod const_value;
mod context;
mod emit_type;
mod func;
mod prog;
mod repr;
mod symbol_table;
mod type_manager;
mod value;

pub fn emit(ir: &caer_ir::module::Module) {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let llctx = inkwell::context::Context::create();
    let llmod = llctx.create_module("main");
    let llbuild = llctx.create_builder();

    sub(&llctx, llmod, llbuild, ir)
}

fn sub<'ctx, 'ir: 'ctx>(
    llctx: &'ctx inkwell::context::Context, llmod: inkwell::module::Module<'ctx>,
    llbuild: inkwell::builder::Builder<'ctx>, ir: &'ir caer_ir::module::Module,
) {
    let emit_ctx = context::Context::new(llctx, llmod, llbuild);

    {
        let mut builder = prog::ProgEmit::new(&emit_ctx, ir);

        builder.emit();

        // not really run, just prints out crap
        // TODO: expose seperately, don't hardcode paths
        builder.run(true);
    }
}
