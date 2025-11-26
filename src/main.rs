//! RustScript compiler command-line interface.
//! 
//! Author: Michael Lauzon
//! 
//! This is the main entry point for the RustScript compiler. It provides a
//! command-line interface for compiling RustScript programmes to various targets.
//! 
//! # Usage
//! 
//! ```bash
//! # Compile to JavaScript
//! rjsc input.rjsc
//! 
//! # Compile to WebAssembly
//! rjsc input.rjsc --target wasm
//! 
//! # Specify output location
//! rjsc input.rjsc --output dist/output.js
//! ```

use std::path::PathBuf;
use clap::Parser;

mod ast;
mod lexer;
mod parser;
mod typechecker;
mod compiler;
mod codegen_wasm;
mod borrow_checker;
mod memory;
mod diagnostics;
mod sourcemap;
mod std_lib;

use crate::compiler::Compiler;

/// RustScript compiler command-line interface.
#[derive(Parser)]
#[command(name = "rjsc")]
#[command(about = "RustScript compiler - Compile RustScript to JavaScript or WebAssembly")]
#[command(version = "0.1.0")]
#[command(author = "Michael Lauzon")]
struct Cli {
    /// Input .rjsc file or directory to compile
    input: PathBuf,
    
    /// Output file or directory (defaults to input name with appropriate extension)
    #[arg(short, long)]
    output: Option<PathBuf>,
    
    /// Compilation target: js, wasm, or native
    #[arg(short, long, default_value = "js")]
    target: String,
    
    /// Watch mode for development (recompile on file changes)
    #[arg(short, long)]
    watch: bool,
    
    /// Generate source maps for debugging
    #[arg(short, long)]
    source_map: bool,
}

impl Cli {
    fn validate_extension(&self) -> anyhow::Result<()> {
        if self.input.is_dir() {
            return Ok(());
        }
        
        match self.input.extension().and_then(|ext| ext.to_str()) {
            Some("rjsc") => Ok(()),
            _ => anyhow::bail!("Input file must have .rjsc extension")
        }
    }
    
    fn determine_output_path(&self) -> PathBuf {
        self.output.clone().unwrap_or_else(|| {
            if self.input.is_dir() {
                PathBuf::from("dist")
            } else {
                let stem = self.input.file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("output");
                match self.target.as_str() {
                    "js" => PathBuf::from(format!("{stem}.js")),
                    "wasm" => PathBuf::from(format!("{stem}.wasm")),
                    "native" => PathBuf::from(stem),
                    _ => PathBuf::from(format!("{stem}.{}", self.target))
                }
            }
        })
    }
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    
    // Validate input file extension
    cli.validate_extension()?;
    
    // Determine project root
    let project_root = if cli.input.is_dir() {
        cli.input.clone()
    } else {
        cli.input.parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."))
    };
    
    let compiler = Compiler::new(project_root);
    
    // Compile the project
    compiler.compile_project(&cli.target)?;
    
    println!("Compilation completed successfully!");
    
    Ok(())
}
