use std::rc::Rc;

mod context;
mod emit_type;
mod func;
mod prog;
mod repr;
mod value;

pub fn emit(ir: &caer_ir::module::Module) {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let llctx = inkwell::context::Context::create();
    let llmod = Rc::new(llctx.create_module("main"));
    let llbuild = Rc::new(llctx.create_builder());

    let emit_ctx = context::Context::new(&llctx, llmod.clone(), llbuild.clone());
    let mut builder = prog::ProgEmit::new(Rc::new(emit_ctx), ir);

    builder.build_funcs();
    builder.emit();

    // not really run, just prints out crap
    // TODO: expose seperately, don't hardcode paths
    builder.run(true);
}
