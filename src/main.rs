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
//! rsxe input.rscc
//! 
//! # Compile to WebAssembly
//! rsxe input.rscc --target wasm
//! 
//! # Specify output location
//! rsxe input.rscc --output dist/output.rscx
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
#[command(name = "rsxe")]
#[command(about = "RustScript Xecutable Engine - Compile RustScript to JavaScript or WebAssembly")]
#[command(version = "0.3.0")]
#[command(author = "Michael Lauzon")]
struct Cli {
    /// Input .rscc file or directory to compile
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
            Some("rscc") => Ok(()),
            _ => anyhow::bail!("Input file must have .rscc extension")
        }
    }
    
    #[allow(dead_code)]
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
    
    if cli.input.is_dir() {
        // Compile all files in directory
        let compiler = Compiler::new(cli.input.clone());
        compiler.compile_project(&cli.target)?;
    } else {
        // Compile single file
        let project_root = cli.input.parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));
        let compiler = Compiler::new(project_root);
        compiler.compile_file(&cli.input, &cli.target)?;
    }
    
    println!("Compilation completed successfully!");
    
    Ok(())
}
