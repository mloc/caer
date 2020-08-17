fn main() {
    let env = caer_frontend::build("main.dm");
    println!("CFG BUILT");

    caer_codegen_llvm::emit(&env);
    println!("EMIT DONE");
}
