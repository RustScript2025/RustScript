//! Compiler orchestration and file resolution.
//! 
//! Author: Michael Lauzon
//! 
//! This module handles the high-level compilation process, including locating
//! source files, resolving imports, and coordinating the various compilation
//! stages (parsing, type checking, and code generation).

use std::path::{Path, PathBuf};
use glob::glob;

/// Resolves file paths and imports within a RustScript project.
/// 
/// The file resolver finds all RustScript files in a project directory
/// and resolves relative imports between modules. It uses glob patterns
/// for efficient source file discovery.
pub struct FileResolver {
    /// The project's root directory.
    pub project_root: PathBuf,
}

impl FileResolver {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }

    pub fn find_rustscript_files(&self) -> anyhow::Result<Vec<PathBuf>> {
        let pattern = self.project_root.join("**/*.rjsc");
        let pattern_str = pattern.to_str()
            .ok_or_else(|| anyhow::anyhow!("Invalid path pattern"))?;
        let paths: Vec<PathBuf> = glob(pattern_str)?
            .filter_map(Result::ok)
            .collect();
        Ok(paths)
    }
}

use crate::diagnostics::DiagnosticManager;

/// The main compiler driver.
/// 
/// The compiler coordinates all stages of compilation, from file discovery
/// through code generation. It maintains diagnostic information and handles
/// error reporting.
pub struct Compiler {
    pub file_resolver: FileResolver,
    #[allow(dead_code)]
    diagnostics: DiagnosticManager,
}

impl Compiler {
    pub fn new(project_root: PathBuf) -> Self {
        Self {
            file_resolver: FileResolver::new(project_root),
            diagnostics: DiagnosticManager::new(),
        }
    }

    /// Compiles all RustScript files in the project to the specified target.
    /// 
    /// Supported targets:
    /// - "js" - JavaScript (ES2020+)
    /// - "wasm" - WebAssembly binary
    /// - "native" - Native executable (future)
    pub fn compile_project(&self, target: &str) -> anyhow::Result<()> {
        let files = self.file_resolver.find_rustscript_files()?;
        
        if files.is_empty() {
            anyhow::bail!("No .rjsc files found in project");
        }

        println!("Found {} file(s) to compile", files.len());

        for file in files {
            match target {
                "js" => self.generate_js(&file)?,
                "wasm" => self.generate_wasm(&file)?,
                "native" => self.generate_native(&file)?,
                _ => anyhow::bail!("Unknown target: {}", target),
            }
        }

        Ok(())
    }

    fn generate_js(&self, file_path: &Path) -> anyhow::Result<()> {
        let output_path = self.get_output_path(file_path, "js");
        println!("Would generate JS: {}", output_path.display());
        Ok(())
    }

    fn generate_wasm(&self, file_path: &Path) -> anyhow::Result<()> {
        use std::fs;
        
        let output_path = self.get_output_path(file_path, "wasm");
        
        // Read source file
        let source = fs::read_to_string(file_path)?;
        
        // Parse using pest
        let ast = crate::parser::parse_program(&source)
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
        
        // Borrow check
        let mut borrow_checker = crate::borrow_checker::BorrowChecker::new();
        if let Err(errors) = borrow_checker.check_module(&ast) {
            let error_msg = errors.join("\n");
            anyhow::bail!("Borrow check failed:\n{}", error_msg);
        }
        
        // Type check
        let mut type_checker = crate::typechecker::TypeChecker::new();
        let expr_types = type_checker.check(&ast)
            .map_err(|e| anyhow::anyhow!("Type error: {}", e))?;
        
        // Generate WASM
        let generator = crate::codegen_wasm::WasmGenerator::new();
        let wasm_bytes = generator.generate(&ast, expr_types)
            .map_err(|e| anyhow::anyhow!("Code generation error: {}", e))?;
        
        // Write output file
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&output_path, wasm_bytes)?;
        println!("Generated WASM: {}", output_path.display());
        
        Ok(())
    }
    
    fn generate_native(&self, file_path: &Path) -> anyhow::Result<()> {
        let output_path = self.get_output_path(file_path, "");
        println!("Would generate native: {}", output_path.display());
        Ok(())
    }
    
    fn get_output_path(&self, file_path: &Path, extension: &str) -> PathBuf {
        let relative_path = pathdiff::diff_paths(file_path, &self.file_resolver.project_root)
            .unwrap_or_else(|| file_path.to_path_buf());
        
        let output_dir = self.file_resolver.project_root.join("dist");
        let mut output_path = output_dir.join(relative_path);
        
        if !extension.is_empty() {
            output_path.set_extension(extension);
        }
        
        output_path
    }
}
