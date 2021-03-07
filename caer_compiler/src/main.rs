fn main() {
    let src_file = std::env::args().skip(1).next().expect("no source file passed");
    let env = caer_frontend::build(src_file);
    println!("CFG BUILT");

    caer_codegen_llvm::emit(&env);
    println!("EMIT DONE");
}
