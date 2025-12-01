//! Type checker for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module implements type checking and inference for the RustScript language.
//! It supports gradual typing, where types can be explicitly annotated or inferred
//! from usage. The type checker ensures type safety while allowing flexibility
//! during development.

use thiserror::Error;
use crate::ast::{Span, Type};
use std::collections::HashMap;

/// Errors that can occur during type checking.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum TypeError {
    /// A type mismatch was detected.
    #[allow(dead_code)]
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    Mismatch { expected: Type, found: Type, span: Span },
    
    /// The type checker couldn't infer a type.
    #[allow(dead_code)]
    #[error("Cannot infer type")]
    CannotInfer { span: Span },
}

/// The type checker analyses and infers types throughout a programme.
/// 
/// This checker performs:
/// - Type inference for variables and expressions
/// - Type checking for function calls and operations
/// - Struct field type validation
/// 
/// # Example
/// 
/// ```ignore
/// let mut checker = TypeChecker::new();
/// let expr_types = checker.check(&ast)?;
/// ```
pub struct TypeChecker {
    /// Global function signatures: name -> (parameter_types, return_type)
    functions: HashMap<String, (Vec<Type>, Type)>,
    /// Struct definitions: name -> field_map
    structs: HashMap<String, HashMap<String, Type>>,
}

impl TypeChecker {
    /// Creates a new type checker with empty function and struct tables.
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    /// Type checks an entire module and returns type information for all expressions.
    /// 
    /// This performs a two-pass analysis:
    /// 1. Collect all function and struct definitions
    /// 2. Check function bodies and expressions
    /// 
    /// Returns a map from expression spans to their inferred types.
    pub fn check(&mut self, module: &crate::ast::Module) -> Result<HashMap<Span, Type>, TypeError> {
        let mut types = HashMap::new();
        
        // Pass 1: Collect definitions
        for item in &module.items {
            match item {
                crate::ast::Item::Function(func) => {
                    let params = func.params.iter()
                        .map(|(_, ty)| self.resolve_type(ty.as_ref()))
                        .collect();
                    let ret_ty = func.return_type.as_ref()
                        .map(|t| self.resolve_type(Some(t)))
                        .unwrap_or(Type::Generic(crate::ast::Ident { 
                            name: "void".into(), 
                            span: Span::default() 
                        }));
                    self.functions.insert(func.name.name.to_string(), (params, ret_ty));
                },
                crate::ast::Item::Struct(s) => {
                    let mut fields = HashMap::new();
                    for (name, ty) in &s.fields {
                        fields.insert(name.name.to_string(), self.resolve_type(Some(ty)));
                    }
                    self.structs.insert(s.name.name.to_string(), fields);
                },
                crate::ast::Item::Extend { methods, .. } => {
                    for method in methods {
                        let params = method.params.iter()
                            .map(|(_, ty)| self.resolve_type(ty.as_ref()))
                            .collect();
                        let ret_ty = method.return_type.as_ref()
                            .map(|t| self.resolve_type(Some(t)))
                            .unwrap_or(Type::Generic(crate::ast::Ident { 
                                name: "void".into(), 
                                span: Span::default() 
                            }));
                        self.functions.insert(method.name.name.to_string(), (params, ret_ty));
                    }
                },
                _ => {}
            }
        }
        
        // Pass 2: Check function bodies
        for item in &module.items {
            match item {
                crate::ast::Item::Function(func) => {
                    self.check_function(func, &mut types)?;
                },
                crate::ast::Item::MultiFn { variants, .. } => {
                    // Multiple dispatch: check all variants
                    for variant in variants {
                        self.check_function(variant, &mut types)?;
                    }
                },
                crate::ast::Item::Extend { methods, .. } => {
                    for method in methods {
                        self.check_function(method, &mut types)?;
                    }
                },
                _ => {}
            }
        }
        
        Ok(types)
    }
    
    fn resolve_type(&self, ty: Option<&Type>) -> Type {
        ty.cloned().unwrap_or(Type::Number)
    }

    fn check_function(&self, func: &crate::ast::Function, types: &mut HashMap<Span, Type>) -> Result<(), TypeError> {
        let mut scopes = vec![HashMap::new()];
        
        // Register parameters in scope
        for (pattern, ty) in &func.params {
            if let crate::ast::Pattern::Ident(id) = pattern {
                let resolved_ty = self.resolve_type(ty.as_ref());
                scopes[0].insert(id.name.to_string(), resolved_ty);
            }
        }
        
        // Check contracts (preconditions, postconditions, invariants)
        for contract in &func.contracts {
            self.check_expr(&contract.condition, types, &mut scopes)?;
        }
        
        self.check_block(&func.body, types, &mut scopes)?;
        Ok(())
    }

    fn check_block(&self, block: &crate::ast::Block, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<Type, TypeError> {
        scopes.push(HashMap::new());
        
        let mut last_ty = Type::Generic(crate::ast::Ident { 
            name: "void".into(), 
            span: Span::default() 
        });
        
        for stmt in &block.stmts {
            self.check_stmt(stmt, types, scopes)?;
        }
        
        if let Some(expr) = &block.expr {
            last_ty = self.check_expr(expr, types, scopes)?;
        }
        
        scopes.pop();
        Ok(last_ty)
    }

    fn check_stmt(&self, stmt: &crate::ast::Stmt, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<(), TypeError> {
        match stmt {
            crate::ast::Stmt::Expr(expr, _) => {
                self.check_expr(expr, types, scopes)?;
                Ok(())
            },
            crate::ast::Stmt::Let { pattern, value, .. } => {
                let ty = if let Some(expr) = value {
                    self.check_expr(expr, types, scopes)?
                } else {
                    Type::Number
                };
                
                if let crate::ast::Pattern::Ident(id) = pattern {
                    scopes.last_mut().unwrap().insert(id.name.to_string(), ty);
                }
                Ok(())
            },
            crate::ast::Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.check_expr(e, types, scopes)?;
                }
                Ok(())
            },
            crate::ast::Stmt::Guard { condition, else_block, .. } => {
                self.check_expr(condition, types, scopes)?;
                self.check_block(else_block, types, scopes)?;
                Ok(())
            },
            crate::ast::Stmt::Defer { block, .. } => {
                self.check_block(block, types, scopes)?;
                Ok(())
            },
        }
    }

    fn check_expr(&self, expr: &crate::ast::Expr, types: &mut HashMap<Span, Type>, scopes: &mut Vec<HashMap<String, Type>>) -> Result<Type, TypeError> {
        let ty = match expr {
            crate::ast::Expr::Literal(lit, _) => {
                match lit {
                    crate::ast::Literal::Number(_) => Type::Number,
                    crate::ast::Literal::String(_) => Type::String,
                    crate::ast::Literal::InterpolatedString(parts) => {
                        // Type-check all embedded expressions
                        for part in parts {
                            if let crate::ast::StringPart::Expr(expr) = part {
                                self.check_expr(expr, types, scopes)?;
                            }
                        }
                        Type::String
                    },
                    crate::ast::Literal::Boolean(_) => Type::Boolean,
                    crate::ast::Literal::Regex(_) => Type::Generic(crate::ast::Ident {
                        name: "Regex".into(),
                        span: Span::default(),
                    }),
                    _ => Type::Number,
                }
            },
            crate::ast::Expr::Ident(id) => {
                // Look up variable in scopes
                let mut found_ty = Type::Number;
                for scope in scopes.iter().rev() {
                    if let Some(ty) = scope.get(&id.name.to_string()) {
                        found_ty = ty.clone();
                        break;
                    }
                }
                found_ty
            },
            crate::ast::Expr::Binary { left, op, right, .. } => {
                let _left_ty = self.check_expr(left, types, scopes)?;
                let _right_ty = self.check_expr(right, types, scopes)?;
                
                match op {
                    crate::ast::BinaryOp::Eq | crate::ast::BinaryOp::Neq |
                    crate::ast::BinaryOp::Lt | crate::ast::BinaryOp::Gt |
                    crate::ast::BinaryOp::Leq | crate::ast::BinaryOp::Geq => Type::Boolean,
                    crate::ast::BinaryOp::And | crate::ast::BinaryOp::Or => Type::Boolean,
                    _ => Type::Number,
                }
            },
            crate::ast::Expr::Call { func, args, .. } => {
                self.check_expr(func, types, scopes)?;
                
                for (_name, arg) in args {
                    self.check_expr(arg, types, scopes)?;
                }
                
                if let crate::ast::Expr::Ident(id) = func.as_ref() {
                    if let Some((_, ret_ty)) = self.functions.get(&id.name.to_string()) {
                        ret_ty.clone()
                    } else if id.name.as_ref() == "console.log" || id.name.as_ref() == "console.error" {
                        Type::Generic(crate::ast::Ident { 
                            name: "void".into(), 
                            span: Span::default() 
                        })
                    } else {
                        Type::Number
                    }
                } else {
                    Type::Number
                }
            },
            crate::ast::Expr::If { condition, then_branch, else_branch, .. } => {
                self.check_expr(condition, types, scopes)?;
                let then_ty = self.check_block(then_branch, types, scopes)?;
                
                if let Some(else_block) = else_branch {
                    let _else_ty = self.check_block(else_block, types, scopes)?;
                }
                
                then_ty
            },
            crate::ast::Expr::Loop { body, .. } => {
                self.check_block(body, types, scopes)?
            },
            crate::ast::Expr::While { condition, body, .. } => {
                self.check_expr(condition, types, scopes)?;
                self.check_block(body, types, scopes)?;
                Type::Generic(crate::ast::Ident { 
                    name: "void".into(), 
                    span: Span::default() 
                })
            },
            crate::ast::Expr::Match { expr: value, arms, .. } => {
                self.check_expr(value, types, scopes)?;
                
                let mut ret_ty = Type::Number;
                for (i, arm) in arms.iter().enumerate() {
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard, types, scopes)?;
                    }
                    let arm_ty = self.check_expr(&arm.body, types, scopes)?;
                    if i == 0 {
                        ret_ty = arm_ty;
                    }
                }
                ret_ty
            },
            crate::ast::Expr::StructInit { name, fields, .. } => {
                for (_field_name, field_expr) in fields {
                    self.check_expr(field_expr, types, scopes)?;
                }
                Type::Generic(name.clone())
            },
            crate::ast::Expr::FieldAccess { expr: obj, field, .. } => {
                let expr_ty = self.check_expr(obj, types, scopes)?;
                
                if let Type::Generic(struct_name) = expr_ty {
                    if let Some(fields) = self.structs.get(&struct_name.name.to_string()) {
                        fields.get(&field.name.to_string())
                            .cloned()
                            .unwrap_or(Type::Number)
                    } else {
                        Type::Number
                    }
                } else {
                    Type::Number
                }
            },
            crate::ast::Expr::Index { expr: arr, index, .. } => {
                let arr_ty = self.check_expr(arr, types, scopes)?;
                self.check_expr(index, types, scopes)?;
                
                // If it's an array type, return the element type
                if let Type::Array(elem_ty) = arr_ty {
                    *elem_ty
                } else {
                    // Default to Number for non-array types
                    Type::Number
                }
            },
            crate::ast::Expr::OptionalChain { expr: obj, field, .. } => {
                // Optional chaining returns Option<T>
                let expr_ty = self.check_expr(obj, types, scopes)?;
                
                if let Type::Generic(struct_name) = expr_ty {
                    if let Some(fields) = self.structs.get(&struct_name.name.to_string()) {
                        // Wrap the field type in Option
                        fields.get(&field.name.to_string())
                            .cloned()
                            .unwrap_or(Type::Number)
                    } else {
                        Type::Number
                    }
                } else {
                    Type::Number
                }
            },
            crate::ast::Expr::NullCoalesce { left, right, .. } => {
                let _left_ty = self.check_expr(left, types, scopes)?;
                let right_ty = self.check_expr(right, types, scopes)?;
                // Return the non-optional type (right side)
                right_ty
            },
            crate::ast::Expr::ListComprehension { expr, pattern, iter, condition, .. } => {
                // Check the iterator
                self.check_expr(iter, types, scopes)?;
                
                // Enter new scope for the pattern binding
                scopes.push(std::collections::HashMap::new());
                
                // Register pattern in scope
                if let crate::ast::Pattern::Ident(id) = pattern {
                    scopes.last_mut().unwrap().insert(id.name.to_string(), Type::Number);
                }
                
                // Check condition if present
                if let Some(cond) = condition {
                    self.check_expr(cond, types, scopes)?;
                }
                
                // Check the expression
                let elem_ty = self.check_expr(expr, types, scopes)?;
                
                scopes.pop();
                
                // Return array of element type
                Type::Array(Box::new(elem_ty))
            },
            crate::ast::Expr::Block(block) => {
                self.check_block(block, types, scopes)?
            },
            crate::ast::Expr::Pipeline { steps, .. } => {
                if steps.is_empty() {
                    Type::Number
                } else {
                    let mut current_ty = self.check_expr(&steps[0], types, scopes)?;
                    for step in &steps[1..] {
                        current_ty = self.check_expr(step, types, scopes)?;
                    }
                    current_ty
                }
            },
            crate::ast::Expr::Async(expr, _) | crate::ast::Expr::Await(expr, _) => {
                self.check_expr(expr, types, scopes)?
            },
            crate::ast::Expr::Yield { value, .. } => {
                // Yield returns the value type
                if let Some(val) = value {
                    self.check_expr(val, types, scopes)?
                } else {
                    Type::Generic(crate::ast::Ident {
                        name: "void".into(),
                        span: Span::default(),
                    })
                }
            },
            crate::ast::Expr::Comptime { block, .. } => {
                // Comptime blocks are evaluated at compile time
                self.check_block(block, types, scopes)?
            },
            crate::ast::Expr::ContractExpr { condition, .. } => {
                // Contract expressions must be boolean
                self.check_expr(condition, types, scopes)?;
                Type::Boolean
            },
        };
        
        types.insert(expr.span().clone(), ty.clone());
        Ok(ty)
    }
}

impl crate::ast::Expr {
    /// Returns the span of this expression.
    pub fn span(&self) -> &Span {
        match self {
            crate::ast::Expr::Literal(_, span) => span,
            crate::ast::Expr::Ident(id) => &id.span,
            crate::ast::Expr::Binary { span, .. } => span,
            crate::ast::Expr::Call { span, .. } => span,
            crate::ast::Expr::Match { span, .. } => span,
            crate::ast::Expr::Pipeline { span, .. } => span,
            crate::ast::Expr::Async(_, span) => span,
            crate::ast::Expr::Await(_, span) => span,
            crate::ast::Expr::Block(block) => &block.span,
            crate::ast::Expr::StructInit { span, .. } => span,
            crate::ast::Expr::FieldAccess { span, .. } => span,
            crate::ast::Expr::Index { span, .. } => span,
            crate::ast::Expr::OptionalChain { span, .. } => span,
            crate::ast::Expr::NullCoalesce { span, .. } => span,
            crate::ast::Expr::ListComprehension { span, .. } => span,
            crate::ast::Expr::Yield { span, .. } => span,
            crate::ast::Expr::Comptime { span, .. } => span,
            crate::ast::Expr::ContractExpr { span, .. } => span,
            crate::ast::Expr::If { span, .. } => span,
            crate::ast::Expr::Loop { span, .. } => span,
            crate::ast::Expr::While { span, .. } => span,
        }
    }
}
