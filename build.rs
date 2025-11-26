// RustScript Build Script
// Author: Michael Lauzon
//
// This build script is currently minimal as pest processes grammars at compile time
// via the pest_derive macro. No build-time grammar generation is needed.

fn main() {
    // Rebuild if grammar changes
    println!("cargo:rerun-if-changed=src/rustscript.pest");
}
