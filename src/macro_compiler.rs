//! Macro Compiler Integration
//! 
//! This module integrates the macro expansion system with the main
//! compilation pipeline, ensuring macros are expanded at the right time.

use crate::ast::*;

use crate::macro_system::*;
use crate::macro_registry::*;

/// Compiler error types
#[derive(Debug)]
pub enum CompilerError {
    ParseError(String),
    MacroError(MacroError),
    TypeError(String),
    BorrowError(String),
    CodegenError(String),
}

impl From<MacroError> for CompilerError {
    fn from(err: MacroError) -> Self {
        CompilerError::MacroError(err)
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            CompilerError::MacroError(err) => write!(f, "Macro error: {}", err),
            CompilerError::TypeError(msg) => write!(f, "Type error: {}", msg),
            CompilerError::BorrowError(msg) => write!(f, "Borrow error: {}", msg),
            CompilerError::CodegenError(msg) => write!(f, "Codegen error: {}", msg),
        }
    }
}

impl std::error::Error for CompilerError {}

/// Macro-aware compilation pipeline
pub struct MacroCompiler {
    macro_phase: MacroExpansionPhase,
    max_expansion_iterations: usize,
}

impl MacroCompiler {
    /// Create a new macro compiler
    pub fn new() -> Self {
        Self {
            macro_phase: MacroExpansionPhase::new(),
            max_expansion_iterations: 10,
        }
    }
    
    /// Expand macros iteratively until no more expansions occur
    pub fn expand_macros_iteratively(&mut self, ast: &mut Vec<Stmt>) -> Result<(), CompilerError> {
        let mut iterations = 0;
        
        loop {
            if iterations >= self.max_expansion_iterations {
                return Err(CompilerError::MacroError(MacroError::RecursionLimit));
            }
            
            let ast_before = format!("{:?}", ast);
            self.macro_phase.expand_macros(ast)?;
            let ast_after = format!("{:?}", ast);
            
            // If AST didn't change, we're done
            if ast_before == ast_after {
                break;
            }
            
            iterations += 1;
        }
        
        Ok(())
    }
    
    /// Process compile-time code generation
    pub fn process_comptime_blocks(&mut self, ast: &mut Vec<Stmt>) -> Result<(), CompilerError> {
        let mut generated_stmts = Vec::new();
        
        for stmt in ast.iter() {
            if let Some(comptime_stmts) = self.extract_comptime_code(stmt)? {
                generated_stmts.extend(comptime_stmts);
            }
        }
        
        // Add generated statements to AST
        ast.extend(generated_stmts);
        
        Ok(())
    }
    
    /// Extract and execute compile-time code blocks
    fn extract_comptime_code(&mut self, _stmt: &Stmt) -> Result<Option<Vec<Stmt>>, CompilerError> {
        // Comptime blocks would be handled here when added to AST
        // For now, return None as they're not yet in the Stmt enum
        Ok(None)
    }
    

    
    /// Process all derive attributes on structs and enums
    pub fn process_derive_attributes(&mut self, _ast: &[Stmt]) -> Result<Vec<Stmt>, CompilerError> {
        // Derive attribute processing would happen here
        // For now, return empty as Struct/Enum are not Stmt variants
        Ok(vec![])
    }
    
    /// Full macro-aware compilation pipeline
    pub fn compile_with_macros(&mut self, mut ast: Vec<Stmt>) -> Result<Vec<Stmt>, CompilerError> {
        // Phase 1: Process compile-time blocks
        self.process_comptime_blocks(&mut ast)?;
        
        // Phase 2: Expand macros iteratively
        self.expand_macros_iteratively(&mut ast)?;
        
        // Phase 3: Process derive attributes
        let derived_impls = self.process_derive_attributes(&ast)?;
        ast.extend(derived_impls);
        
        // Phase 4: Final macro expansion pass
        self.macro_phase.expand_macros(&mut ast)?;
        
        Ok(ast)
    }
}

impl Default for MacroCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_macro_compiler_creation() {
        let compiler = MacroCompiler::new();
        assert_eq!(compiler.max_expansion_iterations, 10);
    }
    
    #[test]
    fn test_empty_ast_compilation() {
        let mut compiler = MacroCompiler::new();
        let ast = vec![];
        let result = compiler.compile_with_macros(ast);
        assert!(result.is_ok());
    }
}
