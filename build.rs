use std::env;
use std::path::Path;

fn main() {
    // Compile LALRPOP grammar
    lalrpop::process_root().unwrap();
    
    // Rebuild if grammar changes
    println!("cargo:rerun-if-changed=src/parser.lalrpop");
}
