mod emit;

use caer_frontend as frontend;

fn main() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let env = frontend::build("main.dm");
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
