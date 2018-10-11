fn main() {
    // Tell cargo to tell rustc to link our match_c library & the c++ dylib.
    // shared library.
    println!("cargo:rustc-link-search=/usr/lib/");
    println!("cargo:rustc-link-lib=dylib=c++");

    println!("cargo:rustc-link-search=../math_iface/gen/");
    println!("cargo:rustc-link-lib=math_c");
}
