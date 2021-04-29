use std::env;
use std::path::PathBuf;

fn main() {
    cc::Build::new()
        .file("src/aco.c")
        .file("src/acosw.S")
        .file("src/shim.c")
        .compile("aco");

    let mut builder = bindgen::Builder::default()
        .header("src/aco.h")
        .header("src/shim.h");

    for ty in &["aco_save_stack_t"] {
        builder = builder.opaque_type(ty);
        builder = builder.whitelist_type(ty);
    }

    for func in &[
        "aco_thread_init",
        "aco_share_stack_new",
        "aco_share_stack_new2",
        "aco_share_stack_destroy",
        "aco_create",
        "aco_destroy",
        "aco_resume",
        "acosw",
        "aco_rshim_get_co",
    ] {
        builder = builder.whitelist_function(func);
    }

    let bindings = builder.generate().expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
