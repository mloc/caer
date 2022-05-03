// jinxing GAT stabilization
#![feature(generic_associated_types)]

mod context;
mod func;
mod prog;
mod repr;
mod rt_type;
mod value;

pub fn emit(ir: &caer_ir::module::Module) {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let llctx = inkwell::context::Context::create();
    let llmod = llctx.create_module("main");
    let llbuild = llctx.create_builder();

    let emit_ctx = context::Context::new(&llctx, &llmod, &llbuild);
    let mut builder = prog::ProgEmit::new(&emit_ctx, ir);
    builder.build_funcs();
    builder.emit();

    // not really run, just prints out crap
    // TODO: expose seperately, don't hardcode paths
    builder.run(true);
}
