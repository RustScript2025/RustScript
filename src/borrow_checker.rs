use crate::ast::{self, Ident, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub enum OwnershipState {
    Owned,
    Moved(Span), // Span where it was moved
    Borrowed(Vec<Span>), // Spans where it is borrowed
}

pub struct BorrowChecker {
    // Map variable name to its current ownership state
    scopes: Vec<HashMap<String, OwnershipState>>,
    errors: Vec<String>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    pub fn check_module(&mut self, module: &ast::Module) -> Result<(), Vec<String>> {
        for item in &module.items {
            if let ast::Item::Function(func) = item {
                self.check_function(func);
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

        // Register params
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
            ast::Stmt::Expr(expr, _) => self.check_expr(expr),
            ast::Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.check_expr(e);
                }
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
                for arg in args {
                    self.check_expr(arg);
                }
            }
            // Other expressions are currently safe or don't involve ownership transfers
            _ => {}
        }
    }

    fn check_use(&mut self, name: &str, span: &Span) {
        // Find variable in scopes (inner to outer)
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get_mut(name) {
                match state {
                    OwnershipState::Moved(moved_span) => {
                        self.errors.push(format!(
                            "Use of moved value '{}' at {:?}. Moved at {:?}",
                            name, span, moved_span
                        ));
                    }
                    OwnershipState::Owned => {
                        // Default to move semantics for all types
                        // (Copy trait analysis would be performed here)
                        *state = OwnershipState::Moved(span.clone());
                    }
                    OwnershipState::Borrowed(_) => {
                        // Borrowed values can be read
                    }
                }
                return;
            }
        }
        // If not found, might be global or error (handled by typechecker)
    }

    fn register_pattern(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Ident(ident) => {
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(ident.name.to_string(), OwnershipState::Owned);
                }
            }
            _ => {
                // Complex pattern binding analysis omitted for brevity
            }
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        // End of scope - owned values are dropped here
    }
}
