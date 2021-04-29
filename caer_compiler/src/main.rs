fn main() {
    let src_file = std::env::args().nth(1).expect("no source file passed");
    let env = caer_frontend::build(src_file);
    println!("CFG BUILT");

    caer_codegen_llvm::emit(&env);

    let shimout = std::fs::File::create("out/shim.dm").unwrap();
    let mut gen = caer_shimgen::Generator::new(std::io::BufWriter::new(shimout));
    gen.generate(&env).unwrap();
    println!("EMIT DONE");
}
