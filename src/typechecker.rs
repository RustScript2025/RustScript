use thiserror::Error;
use crate::ast::{Span, Type};

#[derive(Error, Debug, Clone, PartialEq)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    Mismatch { expected: Type, found: Type, span: Span },
    
    #[error("Cannot infer type")]
    CannotInfer { span: Span },
}

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check(&self, module: &crate::ast::Module) -> Result<(), TypeError> {
        for item in &module.items {
            self.check_item(item)?;
        }
        Ok(())
    }

    fn check_item(&self, item: &crate::ast::Item) -> Result<(), TypeError> {
        match item {
            crate::ast::Item::Function(func) => self.check_function(func),
            _ => Ok(()),
        }
    }

    fn check_function(&self, func: &crate::ast::Function) -> Result<(), TypeError> {
        self.check_block(&func.body)
    }

    fn check_block(&self, block: &crate::ast::Block) -> Result<(), TypeError> {
        for stmt in &block.stmts {
            self.check_stmt(stmt)?;
        }
        if let Some(expr) = &block.expr {
            self.check_expr(expr)?;
        }
        Ok(())
    }

    fn check_stmt(&self, stmt: &crate::ast::Stmt) -> Result<(), TypeError> {
        match stmt {
            crate::ast::Stmt::Expr(expr, _) => self.check_expr(expr),
            crate::ast::Stmt::Let { value: Some(expr), .. } => self.check_expr(expr),
            crate::ast::Stmt::Return(Some(expr), _) => self.check_expr(expr),
            _ => Ok(()),
        }
    }

    fn check_expr(&self, expr: &crate::ast::Expr) -> Result<(), TypeError> {
        match expr {
            crate::ast::Expr::Binary { left, right, .. } => {
                self.check_expr(left)?;
                self.check_expr(right)
            }
            crate::ast::Expr::Call { func, args, .. } => {
                self.check_expr(func)?;
                for arg in args {
                    self.check_expr(arg)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
