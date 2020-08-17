mod context;
mod value;
mod proc;
mod prog;

pub fn emit(ir: &caer_ir::env::Env) {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let llctx = inkwell::context::Context::create();
    let llmod = llctx.create_module("main");
    let llbuild = llctx.create_builder();

    let emit_ctx = context::Context::new(&llctx, &llmod, &llbuild);
    let mut builder = prog::ProgEmit::new(&emit_ctx, ir);
    builder.build_procs();
    builder.emit();

    // not really run, just prints out crap
    // TODO: expose seperately, don't hardcode paths
    builder.run(true);
}
