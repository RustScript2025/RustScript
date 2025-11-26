//! RustScript Build Script
//!
//! Author: Michael Lauzon
//!
//! This build script configures Cargo to rebuild the project when the grammar
//! file changes. The Pest parser generator processes the grammar at compile time
//! via the `pest_derive` macro, so no explicit grammar generation is needed here.
//!
//! The script simply tells Cargo to watch for changes to the grammar file.

fn main() {
    // Instruct Cargo to recompile if the grammar file changes.
    // This ensures the parser stays in sync with grammar modifications.
    println!("cargo:rerun-if-changed=src/rustscript.pest");
}
