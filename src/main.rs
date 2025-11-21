use std::path::PathBuf;
use clap::Parser;

mod ast;
mod lexer;
mod typechecker;
mod compiler;
mod codegen_wasm;
mod borrow_checker;
mod memory;
mod diagnostics;
mod sourcemap;
mod std_lib;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

use crate::compiler::Compiler;

#[derive(Parser)]
#[command(name = "rjsc")]
#[command(about = "RustScript")]
#[command(version = "0.1.0")]
struct Cli {
    /// Input .rjsc file or directory
    input: PathBuf,
    
    /// Output file or directory
    #[arg(short, long)]
    output: Option<PathBuf>,
    
    /// Target: js, wasm, or native (default: js)
    #[arg(short, long, default_value = "js")]
    target: String,
    
    /// Watch mode for development
    #[arg(short, long)]
    watch: bool,
    
    /// Generate source maps
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
                let stem = self.input.file_stem().unwrap();
                match self.target.as_str() {
                    "js" => PathBuf::from(format!("{}.js", stem.to_string_lossy())),
                    "wasm" => PathBuf::from(format!("{}.wasm", stem.to_string_lossy())),
                    "native" => PathBuf::from(stem.to_string_lossy().to_string()),
                    _ => PathBuf::from(format!("{}.{}", stem.to_string_lossy(), self.target))
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
        cli.input.parent().unwrap_or(&PathBuf::from(".")).to_path_buf()
    };
    
    // Create compiler instance
    let compiler = Compiler::new(project_root);
    
    // Compile the project
    compiler.compile_project(&cli.target)?;
    
    println!("Compilation completed successfully!");
    
    Ok(())
}
