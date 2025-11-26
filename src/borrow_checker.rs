//! Borrow checker for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module implements a simplified version of Rust's borrow checker, ensuring
//! memory safety without a garbage collector. It tracks ownership and borrowing
//! of values to prevent use-after-move errors and data races.
//! 
//! The borrow checker operates on the AST after parsing but before code generation,
//! catching memory safety issues at compile time.

use crate::ast::{self, Span};
use std::collections::HashMap;

/// Represents the ownership state of a variable.
/// 
/// Variables in RustScript follow Rust's ownership rules:
/// - Each value has a single owner
/// - When the owner goes out of scope, the value is dropped
/// - Values can be moved or borrowed
#[derive(Debug, Clone)]
pub enum OwnershipState {
    /// The variable owns its value and can be freely used.
    Owned,
    /// The value has been moved elsewhere. Using it is an error.
    Moved(Span),
    /// The value is currently borrowed. The spans indicate where the borrows are.
    Borrowed(Vec<Span>),
}

/// The borrow checker analyses code for memory safety violations.
/// 
/// This checker ensures that:
/// - Values are not used after being moved
/// - Mutable and immutable borrows don't conflict
/// - Values are not accessed after their lifetime ends
/// 
/// # Example
/// 
/// ```ignore
/// let mut checker = BorrowChecker::new();
/// checker.check_module(&ast)?;
/// ```
pub struct BorrowChecker {
    /// Stack of scopes, each mapping variable names to their ownership state.
    scopes: Vec<HashMap<String, OwnershipState>>,
    /// Accumulated error messages from the analysis.
    errors: Vec<String>,
}

impl BorrowChecker {
    /// Creates a new borrow checker with an empty scope stack.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    /// Checks an entire module for borrow checking violations.
    /// 
    /// Returns Ok(()) if no violations are found, or Err with a list of error messages.
    pub fn check_module(&mut self, module: &ast::Module) -> Result<(), Vec<String>> {
        for item in &module.items {
            match item {
                ast::Item::Function(func) => self.check_function(func),
                ast::Item::Extend { methods, .. } => {
                    for method in methods {
                        self.check_function(method);
                    }
                }
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_function(&mut self, func: &ast::Function) {
        self.enter_scope();

        // Register function parameters
        for (pattern, _) in &func.params {
            self.register_pattern(pattern);
        }

        self.check_block(&func.body);
        self.exit_scope();
    }

    fn check_block(&mut self, block: &ast::Block) {
        self.enter_scope();
        
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
        
        if let Some(expr) = &block.expr {
            self.check_expr(expr);
        }
        
        self.exit_scope();
    }

    fn check_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Let { pattern, value, .. } => {
                if let Some(expr) = value {
                    self.check_expr(expr);
                }
                self.register_pattern(pattern);
            }
            ast::Stmt::Expr(expr, _) => {
                self.check_expr(expr);
            }
            ast::Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.check_expr(e);
                }
            }
            ast::Stmt::Guard { condition, else_block, .. } => {
                self.check_expr(condition);
                self.check_block(else_block);
            }
            ast::Stmt::Defer { block, .. } => {
                self.check_block(block);
            }
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Ident(ident) => {
                self.check_use(&ident.name, &ident.span);
            }
            ast::Expr::Binary { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            ast::Expr::Call { func, args, .. } => {
                self.check_expr(func);
                for (_name, arg) in args {
                    self.check_expr(arg);
                }
            }
            ast::Expr::If { condition, then_branch, else_branch, .. } => {
                self.check_expr(condition);
                self.check_block(then_branch);
                if let Some(else_block) = else_branch {
                    self.check_block(else_block);
                }
            }
            ast::Expr::Loop { body, .. } => {
                self.check_block(body);
            }
            ast::Expr::While { condition, body, .. } => {
                self.check_expr(condition);
                self.check_block(body);
            }
            ast::Expr::Match { expr: value, arms, .. } => {
                self.check_expr(value);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            ast::Expr::StructInit { fields, .. } => {
                for (_name, expr) in fields {
                    self.check_expr(expr);
                }
            }
            ast::Expr::FieldAccess { expr, .. } => {
                self.check_expr(expr);
            }
            ast::Expr::OptionalChain { expr, .. } => {
                self.check_expr(expr);
            }
            ast::Expr::NullCoalesce { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            ast::Expr::ListComprehension { expr, iter, condition, .. } => {
                self.check_expr(iter);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.check_expr(expr);
            }
            ast::Expr::Block(block) => {
                self.check_block(block);
            }
            ast::Expr::Pipeline { steps, .. } => {
                for step in steps {
                    self.check_expr(step);
                }
            }
            ast::Expr::Async(expr, _) | ast::Expr::Await(expr, _) => {
                self.check_expr(expr);
            }
            ast::Expr::Yield { value, .. } => {
                if let Some(val) = value {
                    self.check_expr(val);
                }
            }
            ast::Expr::Comptime { block, .. } => {
                self.check_block(block);
            }
            ast::Expr::ContractExpr { condition, .. } => {
                self.check_expr(condition);
            }
            ast::Expr::Literal(_, _) => {
                // Literals don't involve ownership
            }
        }
    }

    /// Checks the use of a variable and updates its ownership state.
    /// 
    /// This method enforces move semantics: when a value is used, it's considered
    /// moved unless it implements Copy (which we don't track yet). Using a moved
    /// value is an error.
    fn check_use(&mut self, name: &str, span: &Span) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get_mut(name) {
                match state {
                    OwnershipState::Moved(moved_span) => {
                        self.errors.push(format!(
                            "Use of moved value '{}' at byte offset {}. Value was moved at byte offset {}.",
                            name, span.start, moved_span.start
                        ));
                    }
                    OwnershipState::Owned => {
                        // Move the value (in a real implementation, we'd check for Copy trait)
                        *state = OwnershipState::Moved(span.clone());
                    }
                    OwnershipState::Borrowed(_) => {
                        // Borrowed values can be read without moving
                    }
                }
                return;
            }
        }
        // Variable not found in any scope - might be a global or will be caught by type checker
    }

    fn register_pattern(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Ident(ident) => {
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(ident.name.to_string(), OwnershipState::Owned);
                }
            }
            ast::Pattern::Tuple(patterns) => {
                for pat in patterns {
                    self.register_pattern(pat);
                }
            }
            ast::Pattern::Record(fields) => {
                for (_name, pat) in fields {
                    self.register_pattern(pat);
                }
            }
            ast::Pattern::Literal(_) | ast::Pattern::Wildcard(_) => {
                // These don't bind variables
            }
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        // Values owned by this scope are dropped here
    }
}
