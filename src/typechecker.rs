use thiserror::Error;
use crate::ast::{Span, Type};
use std::collections::HashMap;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    Mismatch { expected: Type, found: Type, span: Span },
    
    #[error("Cannot infer type")]
    CannotInfer { span: Span },
}

pub struct TypeChecker {
    // Global definitions
    functions: HashMap<String, (Vec<Type>, Type)>, // (params, return_type)
    structs: HashMap<String, HashMap<String, Type>>, // name -> { field -> type }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn check(&mut self, module: &crate::ast::Module) -> Result<HashMap<Span, Type>, TypeError> {
        let mut types = HashMap::new();
        
        // Pass 1: Collect definitions
        for item in &module.items {
            match item {
                crate::ast::Item::Function(func) => {
                    let params = func.params.iter().map(|(_, ty)| self.resolve_type(ty)).collect();
                    let ret_ty = func.return_type.as_ref().map(|t| self.resolve_type(Some(t))).unwrap_or(Type::Generic(crate::ast::Ident { name: "void".into(), span: Span::default() }));
                    self.functions.insert(func.name.name.to_string(), (params, ret_ty));
                },
                crate::ast::Item::Struct(s) => {
                    let mut fields = HashMap::new();
                    for (name, ty) in &s.fields {
                        fields.insert(name.name.to_string(), self.resolve_type(Some(ty)));
                    }
                    self.structs.insert(s.name.name.to_string(), fields);
                },
                _ => {}
            }
        }
        
        // Pass 2: Check bodies
        for item in &module.items {
            if let crate::ast::Item::Function(func) = item {
                self.check_function(func, &mut types)?;
            }
        }
        
        Ok(types)
    }
    
    fn resolve_type(&self, ty: Option<&Type>) -> Type {
        ty.cloned().unwrap_or(Type::Number) // Default to Number if missing
    }

    fn check_function(&self, func: &crate::ast::Function, types: &mut HashMap<Span, Type>) -> Result<(), TypeError> {
        let mut scopes = vec![HashMap::new()];
        
        // Register params in scope
        for (pattern, ty) in &func.params {
            if let crate::ast::Pattern::Ident(id) = pattern {
                let resolved_ty = self.resolve_type(ty.as_ref());
                scopes[0].insert(id.name.to_string(), resolved_ty);
            }
        }
        
        self.check_block(&func.body, types, &mut scopes)
    }

    fn check_block(&self, block: &crate::ast::Block, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<Type, TypeError> {
        scopes.push(HashMap::new()); // Enter scope
        
        let mut last_ty = Type::Generic(crate::ast::Ident { name: "void".into(), span: Span::default() });
        
        for stmt in &block.stmts {
            self.check_stmt(stmt, types, scopes)?;
        }
        if let Some(expr) = &block.expr {
            last_ty = self.check_expr(expr, types, scopes)?;
        }
        
        scopes.pop(); // Exit scope
        Ok(last_ty)
    }

    fn check_stmt(&self, stmt: &crate::ast::Stmt, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<(), TypeError> {
        match stmt {
            crate::ast::Stmt::Expr(expr, _) => { self.check_expr(expr, types, scopes)?; Ok(()) },
            crate::ast::Stmt::Let { pattern, value, span: _, type_ann: _, mutable: _ } => {
                let ty = if let Some(expr) = value {
                    self.check_expr(expr, types, scopes)?
                } else {
                    Type::Number // Default?
                };
                

    fn check_expr(&self, expr: &crate::ast::Expr, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<Type, TypeError> {
        let ty = match expr {
            crate::ast::Expr::Binary { left, op, right, span: _ } => {
                let left_ty = self.check_expr(left, types, scopes)?;
                let right_ty = self.check_expr(right, types, scopes)?;
                // Basic type checking for binary ops
                match op {
                    crate::ast::BinaryOp::Eq | crate::ast::BinaryOp::Lt | crate::ast::BinaryOp::Gt => Type::Boolean,
                    _ => Type::Number, // Assume arithmetic returns Number
                }
            }
            crate::ast::Expr::Call { func, args, span: _ } => {
                self.check_expr(func, types, scopes)?;
                for arg in args {
                    self.check_expr(arg, types, scopes)?;
                }
                
                // Look up function return type
                if let crate::ast::Expr::Ident(id) = func.as_ref() {
                    if let Some((_, ret_ty)) = self.functions.get(&id.name.to_string()) {
                        ret_ty.clone()
                    } else {
                        // Check for built-ins
                        if id.name.as_ref() == "console.log" || id.name.as_ref() == "console.error" {
                             Type::Generic(crate::ast::Ident { name: "void".into(), span: Span::default() })
                        } else {
                            Type::Number // Fallback
                        }
                    }
                } else {
                    Type::Number
                }
            }
            crate::ast::Expr::Literal(lit, _) => match lit {
                crate::ast::Literal::Number(_) => Type::Number,
                crate::ast::Literal::String(_) => Type::String,
                crate::ast::Literal::Boolean(_) => Type::Boolean,
                _ => Type::Number,
            },
            crate::ast::Expr::Ident(id) => {
                // Look up variable in scopes
                let mut found_ty = Type::Number; // Default
                for scope in scopes.iter().rev() {
                    if let Some(ty) = scope.get(&id.name.to_string()) {
                        found_ty = ty.clone();
                        break;
                    }
                }
                found_ty
            },
            crate::ast::Expr::StructInit { name, fields, span: _ } => {
                for (_, expr) in fields {
                    self.check_expr(expr, types, scopes)?;
                }
                Type::Generic(name.clone())
            },
            crate::ast::Expr::FieldAccess { expr, field, span: _ } => {
                 let expr_ty = self.check_expr(expr, types, scopes)?;
                 if let Type::Generic(struct_name) = expr_ty {
                     if let Some(fields) = self.structs.get(&struct_name.name.to_string()) {
                         fields.get(&field.name.to_string()).cloned().unwrap_or(Type::Number)
                     } else {
                         Type::Number
                     }
                 } else {
                     Type::Number
                 }
            },
            crate::ast::Expr::If { condition, then_branch, else_branch, span: _ } => {
                self.check_expr(condition, types, scopes)?;
                let then_ty = self.check_block(then_branch, types, scopes)?;
                if let Some(else_block) = else_branch {
                    let else_ty = self.check_block(else_block, types, scopes)?;
                    then_ty // Should unify
                } else {
                    then_ty
                }
            },
            crate::ast::Expr::Loop { body, span: _ } => {
                self.check_block(body, types, scopes)?
            },
            crate::ast::Expr::While { condition, body, span: _ } => {
                self.check_expr(condition, types, scopes)?;
                self.check_block(body, types, scopes)?;
                Type::Generic(crate::ast::Ident { name: "void".into(), span: crate::ast::Span::default() })
            },
            crate::ast::Expr::Match { expr: value, arms, span: _ } => {
                self.check_expr(value, types, scopes)?;
                let mut ret_ty = Type::Number; // Default
                for (i, arm) in arms.iter().enumerate() {
                    // Match arms introduce a scope if they bind variables
                    // For now, we don't support binding in this checker, but we should check the body
                    let arm_ty = self.check_expr(&arm.body, types, scopes)?;
                    if i == 0 { ret_ty = arm_ty; }
                }
                ret_ty
            },
            _ => Type::Number,
        };
        
        // Record type for this expression's span
        types.insert(expr.span().clone(), ty.clone());
        
        Ok(ty)
    }
}
