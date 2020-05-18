fn main() {
    println!("cargo:rustc-link-lib=dylib=ludo");
    println!("cargo:rustc-link-search=native=.");
}
