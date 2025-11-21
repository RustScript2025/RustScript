use std::path::{Path, PathBuf};
use glob::glob;
use crate::ast::FileType;

/// Resolves file paths and imports within a RustScript project.
pub struct FileResolver {
    project_root: PathBuf,
}

    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }

    pub fn find_rustscript_files(&self) -> anyhow::Result<Vec<PathBuf>> {
        let pattern = self.project_root.join("**/*.rjsc");
        let paths: Vec<PathBuf> = glob(pattern.to_str().unwrap())?
            .filter_map(Result::ok)
            .collect();
        Ok(paths)
    }
}

use crate::diagnostics::DiagnosticManager;

/// The main compiler driver.
pub struct Compiler {
    file_resolver: FileResolver,
    diagnostics: DiagnosticManager,
}

impl Compiler {
    pub fn new(project_root: PathBuf) -> Self {
        Self {
            file_resolver: FileResolver::new(project_root),
            diagnostics: DiagnosticManager::new(),
        }
    }
    
    pub fn compile_project(&self, target: &str) -> anyhow::Result<()> {
        let files = self.file_resolver.find_rustscript_files()?;
        println!("Found {} RustScript files", files.len());
        for file in files {
            println!("Compiling: {}", file.display());
            self.compile_file(&file, target)?;
        }
        Ok(())
    }
    
    fn compile_file(&self, file_path: &Path, target: &str) -> anyhow::Result<()> {
        let source = std::fs::read_to_string(file_path)?;
        
        match target {
            "js" => self.generate_javascript(file_path, &source)?,
            "wasm" => self.generate_wasm(file_path)?,
            "native" => self.generate_native(file_path)?,
            _ => anyhow::bail!("Unknown target: {}", target),
        }
        Ok(())
    }
    
    fn generate_javascript(&self, file_path: &Path, source: &str) -> anyhow::Result<()> {
        let output_path = self.get_output_path(file_path, "js");
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let js_code = format!("// Compiled from RustScript\n// Features: Named Args, Guard, Defer, Extend, Regex Literals\n\n{}", 
                             source);
        std::fs::write(&output_path, js_code)?;
        println!("Generated: {}", output_path.display());
        Ok(())
    }
    
    fn generate_wasm(&self, file_path: &Path) -> anyhow::Result<()> {
        let output_path = self.get_output_path(file_path, "wasm");
        
        // Read source
        let source = std::fs::read_to_string(file_path)?;
        
        // 1. Parse
        // Parse source
        let parser = crate::parser::ProgramParser::new();
        let ast = parser.parse(&source)
            .map_err(|e| anyhow::anyhow!("Parse Error: {:?}", e))?;

        // 2. Safety Checks (Borrow Checker)
        let mut borrow_checker = crate::borrow_checker::BorrowChecker::new();
        if let Err(errors) = borrow_checker.check_module(&ast) {
            let error_msg = errors.join("\n");
            anyhow::bail!("Borrow Check Error:\n{}", error_msg);
        }
        
        // 3. Type Check
        let type_checker = crate::typechecker::TypeChecker::new();
        let expr_types = type_checker.check(&ast)
            .map_err(|e| anyhow::anyhow!("Type Error: {}", e))?;

        // 4. Code Generation (WASM)
        let generator = crate::codegen_wasm::WasmGenerator::new();
        let wasm_bytes = generator.generate(&ast, expr_types)
            .map_err(|e| anyhow::anyhow!("Codegen Error: {}", e))?;
        
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
