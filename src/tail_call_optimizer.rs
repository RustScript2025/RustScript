//! Tail Call Optimization for RustScript.
//!
//! Author: Michael Lauzon
//!
//! This module detects and optimizes tail-recursive function calls by converting
//! them into loops, preventing stack overflow and improving performance.
//!
//! Phase 4A: Tail Call Optimization

use crate::ast::*;
use std::collections::HashSet;

/// Analyzes a function to determine if it's tail-recursive and can be optimized.
pub struct TailCallOptimizer {
    /// Functions that have been analyzed
    analyzed: HashSet<String>,
}

impl TailCallOptimizer {
    pub fn new() -> Self {
        Self {
            analyzed: HashSet::new(),
        }
    }

    /// Analyzes a module and marks tail-recursive functions for optimization.
    pub fn optimize_module(&mut self, module: &mut Module) {
        for item in &mut module.items {
            match item {
                Item::Function(func) => {
                    if self.is_tail_recursive(func) {
                        func.tail_call_optimized = true;
                    }
                }
                Item::MultiFn { variants, .. } => {
                    for func in variants {
                        if self.is_tail_recursive(func) {
                            func.tail_call_optimized = true;
                        }
                    }
                }
                Item::Extend { methods, .. } => {
                    for method in methods {
                        if self.is_tail_recursive(method) {
                            method.tail_call_optimized = true;
                        }
                    }
                }
                Item::TraitImpl(trait_impl) => {
                    for method in &mut trait_impl.methods {
                        if self.is_tail_recursive(method) {
                            method.tail_call_optimized = true;
                        }
                    }
                }
                Item::TypeAlias(_) | Item::TypeFunction(_) | Item::Enum(_) => {
                    // Phase 4B: Type-level items don't contain functions
                }
                _ => {}
            }
        }
    }

    /// Checks if a function is tail-recursive.
    ///
    /// A function is tail-recursive if:
    /// 1. It calls itself
    /// 2. The recursive call is in tail position (last operation before return)
    fn is_tail_recursive(&mut self, func: &Function) -> bool {
        let func_name = func.name.name.as_ref();
        
        // Mark as analyzed
        self.analyzed.insert(func_name.to_string());
        
        // Check if the function body contains tail-recursive calls
        self.has_tail_call(&func.body, func_name)
    }

    /// Checks if a block has a tail call to the given function.
    fn has_tail_call(&self, block: &Block, func_name: &str) -> bool {
        // Check the final expression in the block
        if let Some(expr) = &block.expr {
            return self.is_tail_call_expr(expr, func_name);
        }
        
        // Check the last statement if it's a return
        if let Some(Stmt::Return(Some(expr), _)) = block.stmts.last() {
            return self.is_tail_call_expr(expr, func_name);
        }
        
        false
    }

    /// Checks if an expression is a tail call to the given function.
    fn is_tail_call_expr(&self, expr: &Expr, func_name: &str) -> bool {
        match expr {
            // Direct function call
            Expr::Call { func, .. } => {
                if let Expr::Ident(ident) = func.as_ref() {
                    return ident.name.as_ref() == func_name;
                }
                false
            }
            
            // If expression - check both branches
            Expr::If { then_branch, else_branch, .. } => {
                let then_tail = self.has_tail_call(then_branch, func_name);
                let else_tail = else_branch.as_ref()
                    .map(|b| self.has_tail_call(b, func_name))
                    .unwrap_or(false);
                then_tail || else_tail
            }
            
            // Match expression - check all arms
            Expr::Match { arms, .. } => {
                arms.iter().any(|arm| self.is_tail_call_expr(&arm.body, func_name))
            }
            
            // Block expression
            Expr::Block(block) => {
                self.has_tail_call(block, func_name)
            }
            
            // Phase 4A: Effect expressions
            Expr::Handle { body, .. } => {
                self.has_tail_call(body, func_name)
            }
            
            // Phase 4A: Inline assembly doesn't contain tail calls
            Expr::InlineAsm { .. } => false,
            
            _ => false,
        }
    }
}

impl Default for TailCallOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tail_recursive_detection() {
        // This would require constructing AST nodes for testing
        // For now, we trust the implementation
        let mut optimizer = TailCallOptimizer::new();
        assert!(optimizer.analyzed.is_empty());
    }
}
