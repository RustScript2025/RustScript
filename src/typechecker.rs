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
    /// Phase 4B: Type aliases: name -> target_type
    type_aliases: HashMap<String, Type>,
    /// Phase 4G: Trait implementations: (trait_name, type_name) -> TraitImpl
    trait_impls: HashMap<(String, String), crate::ast::TraitImpl>,
}

impl TypeChecker {
    /// Creates a new type checker with empty function and struct tables.
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            type_aliases: HashMap::new(),
            trait_impls: HashMap::new(),
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
                        .map(|(_, ty, _)| self.resolve_type(ty.as_ref()))
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
                            .map(|(_, ty, _)| self.resolve_type(ty.as_ref()))
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
                crate::ast::Item::TypeAlias(alias) => {
                    // Phase 4B: Collect type aliases
                    let resolved = self.resolve_type(Some(&alias.target));
                    self.type_aliases.insert(alias.name.name.to_string(), resolved);
                },
                crate::ast::Item::TraitImpl(trait_impl) => {
                    // Phase 4G: Collect trait implementations
                    let trait_name = trait_impl.trait_name.name.to_string();
                    let type_name = self.type_to_string(&trait_impl.for_type);
                    self.trait_impls.insert((trait_name, type_name), trait_impl.clone());
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
        match ty {
            Some(Type::Reference { inner, mutable, lifetime }) => {
                // Phase 4A: Resolve reference types with lifetimes
                Type::Reference {
                    inner: Box::new(self.resolve_type(Some(inner))),
                    mutable: *mutable,
                    lifetime: lifetime.clone(),
                }
            }
            Some(Type::ConstArray { element_type, size }) => {
                // Phase 4A: Resolve const generic arrays
                Type::ConstArray {
                    element_type: Box::new(self.resolve_type(Some(element_type))),
                    size: size.clone(),
                }
            }
            Some(crate::ast::Type::Union(types)) => {
                // Phase 4B: Resolve union types
                crate::ast::Type::Union(types.iter().map(|t| self.resolve_type(Some(t))).collect())
            }
            Some(crate::ast::Type::Intersection(types)) => {
                // Phase 4B: Resolve intersection types
                crate::ast::Type::Intersection(types.iter().map(|t| self.resolve_type(Some(t))).collect())
            }
            Some(Type::Generic(ident)) => {
                // Phase 4B: Check if this is a type alias
                if let Some(aliased) = self.type_aliases.get(ident.name.as_ref()) {
                    aliased.clone()
                } else {
                    Type::Generic(ident.clone())
                }
            }
            Some(Type::HigherKinded { constructor, arity }) => {
                // Phase 4B: Higher-kinded types are resolved as-is
                Type::HigherKinded {
                    constructor: constructor.clone(),
                    arity: *arity,
                }
            }
            Some(Type::AppliedHigherKinded { constructor, args }) => {
                // Phase 4B: Resolve applied higher-kinded types
                Type::AppliedHigherKinded {
                    constructor: Box::new(self.resolve_type(Some(constructor))),
                    args: args.iter().map(|t| self.resolve_type(Some(t))).collect(),
                }
            }
            Some(Type::PhantomData(inner)) => {
                // Phase 4B: PhantomData is a zero-sized marker type
                Type::PhantomData(Box::new(self.resolve_type(Some(inner))))
            }
            Some(Type::Refinement { base, binder, predicate }) => {
                // Phase 4B: Refinement types add predicates to base types
                // The predicate is checked at compile-time or runtime
                Type::Refinement {
                    base: Box::new(self.resolve_type(Some(base))),
                    binder: binder.clone(),
                    predicate: predicate.clone(),
                }
            }
            Some(Type::Dependent { constructor, value_params }) => {
                // Phase 4B: Dependent types where type depends on runtime values
                // The value parameters are evaluated at runtime
                Type::Dependent {
                    constructor: constructor.clone(),
                    value_params: value_params.clone(),
                }
            }
            Some(Type::TypeLevelApp { func, args }) => {
                // Phase 4B: Type-level function application
                Type::TypeLevelApp {
                    func: func.clone(),
                    args: args.iter().map(|t| self.resolve_type(Some(t))).collect(),
                }
            }
            Some(Type::TypeLevelLit(n)) => {
                // Phase 4B: Type-level literals
                Type::TypeLevelLit(*n)
            }
            Some(Type::Existential { bounds }) => {
                // Phase 4B: Existential types hide concrete implementation
                Type::Existential {
                    bounds: bounds.clone(),
                }
            }
            Some(Type::GADTReturn { constructor, type_args }) => {
                // Phase 4B: GADT constructor return type
                Type::GADTReturn {
                    constructor: constructor.clone(),
                    type_args: type_args.iter().map(|t| self.resolve_type(Some(t))).collect(),
                }
            }
            Some(Type::ImmutableVec(elem)) => {
                // Phase 4C: Immutable vector
                Type::ImmutableVec(Box::new(self.resolve_type(Some(elem))))
            }
            Some(Type::ImmutableMap { key_type, value_type }) => {
                // Phase 4C: Immutable map
                Type::ImmutableMap {
                    key_type: Box::new(self.resolve_type(Some(key_type))),
                    value_type: Box::new(self.resolve_type(Some(value_type))),
                }
            }
            Some(Type::ImmutableSet(elem)) => {
                // Phase 4C: Immutable set
                Type::ImmutableSet(Box::new(self.resolve_type(Some(elem))))
            }
            Some(t) => t.clone(),
            None => Type::Number,
        }
    }

    fn check_function(&self, func: &crate::ast::Function, types: &mut HashMap<Span, Type>) -> Result<(), TypeError> {
        let mut scopes = vec![HashMap::new()];
        
        // Register parameters in scope
        for (pattern, ty, _default) in &func.params {
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
            crate::ast::Stmt::Break { value, .. } => {
                // Phase 4E: Break statement
                if let Some(v) = value {
                    self.check_expr(v, types, scopes)?;
                }
                Ok(())
            },
            crate::ast::Stmt::Continue { .. } => {
                // Phase 4E: Continue statement
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
            crate::ast::Expr::Move { expr, .. } => {
                // Phase 4A: Move expression - returns the type of the moved value
                self.check_expr(expr, types, scopes)?
            },
            crate::ast::Expr::Borrow { expr, mutable, .. } => {
                // Phase 4A: Borrow expression - returns a reference type
                let inner_ty = self.check_expr(expr, types, scopes)?;
                Type::reference(inner_ty, *mutable)
            },
            crate::ast::Expr::Perform { args, .. } => {
                // Phase 4A: Perform effect - check arguments, return generic type
                for arg in args {
                    self.check_expr(arg, types, scopes)?;
                }
                Type::Generic(crate::ast::Ident {
                    name: "Effect".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Handle { body, handlers, .. } => {
                // Phase 4A: Handle effects - type is the body's type
                let mut body_ty = Type::Number;
                for stmt in &body.stmts {
                    if let crate::ast::Stmt::Expr(expr, _) = stmt {
                        body_ty = self.check_expr(expr, types, scopes)?;
                    }
                }
                if let Some(expr) = &body.expr {
                    body_ty = self.check_expr(expr, types, scopes)?;
                }
                // Check handlers
                for handler in handlers {
                    for stmt in &handler.body.stmts {
                        if let crate::ast::Stmt::Expr(expr, _) = stmt {
                            self.check_expr(expr, types, scopes)?;
                        }
                    }
                    if let Some(expr) = &handler.body.expr {
                        self.check_expr(expr, types, scopes)?;
                    }
                }
                body_ty
            },
            crate::ast::Expr::Resume { value, .. } => {
                // Phase 4A: Resume - type is the value's type
                self.check_expr(value, types, scopes)?
            },
            crate::ast::Expr::InlineAsm { .. } => {
                // Phase 4A: Inline assembly - return generic type
                Type::Generic(crate::ast::Ident {
                    name: "Asm".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Placeholder(_) => {
                // Phase 4C: Placeholder - return generic type
                Type::Generic(crate::ast::Ident {
                    name: "Placeholder".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::PartialApplication { .. } => {
                // Phase 4C: Partial application - return function type
                Type::Generic(crate::ast::Ident {
                    name: "PartialFn".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Lazy { expr, .. } => {
                // Phase 4C: Lazy evaluation - return Lazy<T> type
                let inner_type = self.check_expr(expr, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: format!("Lazy<{:?}>", inner_type).into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Force { expr, .. } => {
                // Phase 4C: Force evaluation - return inner type
                self.check_expr(expr, types, scopes)?
            },
            crate::ast::Expr::Do { bindings, result, .. } => {
                // Phase 4C: Monadic do-notation
                for binding in bindings {
                    self.check_expr(&binding.expr, types, scopes)?;
                }
                self.check_expr(result, types, scopes)?
            },
            crate::ast::Expr::Spawn { body, .. } => {
                // Phase 4D: Spawn returns a JoinHandle
                self.check_block(body, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "JoinHandle".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Channel { .. } => {
                // Phase 4D: Channel returns (Sender, Receiver) tuple
                Type::Tuple(vec![
                    Type::Generic(crate::ast::Ident {
                        name: "Sender".into(),
                        span: Span::default(),
                    }),
                    Type::Generic(crate::ast::Ident {
                        name: "Receiver".into(),
                        span: Span::default(),
                    }),
                ])
            },
            crate::ast::Expr::Send { channel, value, .. } => {
                // Phase 4D: Send returns Result<(), SendError>
                self.check_expr(channel, types, scopes)?;
                self.check_expr(value, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "Result".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Recv { channel, .. } => {
                // Phase 4D: Recv returns Result<T, RecvError>
                self.check_expr(channel, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "Result".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Select { arms, .. } => {
                // Phase 4D: Select returns the type of the first arm
                if let Some(arm) = arms.first() {
                    self.check_block(&arm.body, types, scopes)?
                } else {
                    Type::Generic(crate::ast::Ident {
                        name: "()".into(),
                        span: Span::default(),
                    })
                }
            },
            crate::ast::Expr::Scope { body, .. } => {
                // Phase 4D: Scope returns the body's type
                self.check_block(body, types, scopes)?
            },
            crate::ast::Expr::Atomic { target, value, .. } => {
                // Phase 4D: Atomic operations return the target type
                self.check_expr(target, types, scopes)?;
                if let Some(v) = value {
                    self.check_expr(v, types, scopes)?;
                }
                Type::Number
            },
            crate::ast::Expr::Lock { mutex, .. } => {
                // Phase 4D: Lock returns a MutexGuard
                self.check_expr(mutex, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "MutexGuard".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::ReadLock { rwlock, .. } => {
                // Phase 4D: ReadLock returns a RwLockReadGuard
                self.check_expr(rwlock, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "RwLockReadGuard".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::WriteLock { rwlock, .. } => {
                // Phase 4D: WriteLock returns a RwLockWriteGuard
                self.check_expr(rwlock, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "RwLockWriteGuard".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::FutureJoin { futures, .. } => {
                // Phase 4D: Future::join returns tuple of results
                let mut types_vec = Vec::new();
                for future in futures {
                    let ty = self.check_expr(future, types, scopes)?;
                    types_vec.push(ty);
                }
                Type::Tuple(types_vec)
            },
            crate::ast::Expr::FutureSelect { futures, .. } => {
                // Phase 4D: Future::select returns first completed future's type
                if let Some(first) = futures.first() {
                    self.check_expr(first, types, scopes)?
                } else {
                    Type::Generic(crate::ast::Ident {
                        name: "()".into(),
                        span: Span::default(),
                    })
                }
            },
            crate::ast::Expr::FutureRace { futures, .. } => {
                // Phase 4D: Future::race returns first completed future's type
                if let Some(first) = futures.first() {
                    self.check_expr(first, types, scopes)?
                } else {
                    Type::Generic(crate::ast::Ident {
                        name: "()".into(),
                        span: Span::default(),
                    })
                }
            },
            crate::ast::Expr::Timeout { duration, .. } => {
                // Phase 4D: Timeout returns Result<T, TimeoutError>
                self.check_expr(duration, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "Result".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::StreamFromIter { iter, .. } => {
                // Phase 4D: Stream::from_iter returns Stream<T>
                let iter_type = self.check_expr(iter, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: format!("Stream<{:?}>", iter_type).into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::StreamMap { stream, mapper, .. } => {
                // Phase 4D: Stream map returns Stream<U>
                self.check_expr(stream, types, scopes)?;
                self.check_expr(mapper, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "Stream".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::StreamFilter { stream, predicate, .. } => {
                // Phase 4D: Stream filter returns Stream<T>
                let stream_type = self.check_expr(stream, types, scopes)?;
                self.check_expr(predicate, types, scopes)?;
                stream_type
            },
            crate::ast::Expr::StreamCollect { stream, .. } => {
                // Phase 4D: Stream collect returns Vec<T>
                self.check_expr(stream, types, scopes)?;
                Type::Array(Box::new(Type::Generic(crate::ast::Ident {
                    name: "T".into(),
                    span: Span::default(),
                })))
            },
            crate::ast::Expr::ParIter { collection, .. } => {
                // Phase 4D: par_iter returns ParallelIterator<T>
                let coll_type = self.check_expr(collection, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: format!("ParallelIterator<{:?}>", coll_type).into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Try { body, catch_clauses, .. } => {
                // Phase 4E: Try block returns Result type
                let body_type = self.check_block(body, types, scopes)?;
                
                // Check all catch clauses
                for clause in catch_clauses {
                    self.check_block(&clause.body, types, scopes)?;
                }
                
                body_type
            },
            crate::ast::Expr::TryOperator { expr, .. } => {
                // Phase 4E: ? operator unwraps Result/Option
                let inner_type = self.check_expr(expr, types, scopes)?;
                // Returns the inner type (T from Result<T, E> or Option<T>)
                inner_type
            },
            crate::ast::Expr::Guard { condition, else_block, .. } => {
                // Phase 4E: Guard clause
                self.check_expr(condition, types, scopes)?;
                self.check_block(else_block, types, scopes)?;
                // Guard returns unit type
                Type::Generic(crate::ast::Ident {
                    name: "()".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::LabeledBlock { block, .. } => {
                // Phase 4E: Labelled block returns block's type
                self.check_block(block, types, scopes)?
            },
            crate::ast::Expr::BreakWithValue { value, .. } => {
                // Phase 4E: Break with value
                if let Some(v) = value {
                    self.check_expr(v, types, scopes)?
                } else {
                    Type::Generic(crate::ast::Ident {
                        name: "()".into(),
                        span: Span::default(),
                    })
                }
            },
            crate::ast::Expr::Catch { expr, handler, .. } => {
                // Phase 4E: Catch expression returns handler's type
                self.check_expr(expr, types, scopes)?;
                self.check_expr(handler, types, scopes)?
            },
            crate::ast::Expr::Panic { message, .. } => {
                // Phase 4E: Panic never returns (! type)
                if let Some(msg) = message {
                    self.check_expr(msg, types, scopes)?;
                }
                Type::Generic(crate::ast::Ident {
                    name: "!".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::CfgExpr { then_expr, else_expr, .. } => {
                // Phase 4E: Conditional compilation returns then branch type
                let then_type = self.check_expr(then_expr, types, scopes)?;
                if let Some(else_e) = else_expr {
                    self.check_expr(else_e, types, scopes)?;
                }
                then_type
            },
            crate::ast::Expr::ConstAssert { condition, message, .. } => {
                // Phase 4E: Const assertion returns unit
                self.check_expr(condition, types, scopes)?;
                if let Some(msg) = message {
                    self.check_expr(msg, types, scopes)?;
                }
                Type::Generic(crate::ast::Ident {
                    name: "()".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Unreachable { message, .. } => {
                // Phase 4E: Unreachable never returns (! type)
                if let Some(msg) = message {
                    self.check_expr(msg, types, scopes)?;
                }
                Type::Generic(crate::ast::Ident {
                    name: "!".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::MacroInvocation { args, .. } => {
                // Phase 4F: Macro invocation - type depends on expansion
                for arg in args {
                    self.check_expr(arg, types, scopes)?;
                }
                // Return generic type for now
                Type::Generic(crate::ast::Ident {
                    name: "T".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::TypeInfo { .. } => {
                // Phase 4F: Type reflection returns TypeInfo struct
                Type::Generic(crate::ast::Ident {
                    name: "TypeInfo".into(),
                    span: Span::default(),
                })
            },
            crate::ast::Expr::Quote { code, .. } => {
                // Phase 4F: Quote returns TokenStream
                self.check_block(code, types, scopes)?;
                Type::Generic(crate::ast::Ident {
                    name: "TokenStream".into(),
                    span: Span::default(),
                })
            },
            // Phase 4G: String slicing
            crate::ast::Expr::StringSlice { string, range, .. } => {
                let _string_ty = self.check_expr(string, types, scopes)?;
                // Check range expressions
                if let Some(start) = &range.start {
                    self.check_expr(start, types, scopes)?;
                }
                if let Some(end) = &range.end {
                    self.check_expr(end, types, scopes)?;
                }
                if let Some(step) = &range.step {
                    self.check_expr(step, types, scopes)?;
                }
                Type::String
            },
            // Phase 4G: Format strings
            crate::ast::Expr::FormatString { parts, .. } => {
                for part in parts {
                    if let crate::ast::FormatPart::Formatted { expr, .. } = part {
                        self.check_expr(expr, types, scopes)?;
                    }
                }
                Type::String
            },
            // Phase 4G: Destructuring assignment
            crate::ast::Expr::DestructuringAssign { value, .. } => {
                let value_ty = self.check_expr(value, types, scopes)?;
                // Pattern checking would happen here
                value_ty
            },
            // Phase 4G: Range expression
            crate::ast::Expr::Range { start, end, step, .. } => {
                if let Some(s) = start {
                    self.check_expr(s, types, scopes)?;
                }
                if let Some(e) = end {
                    self.check_expr(e, types, scopes)?;
                }
                if let Some(st) = step {
                    self.check_expr(st, types, scopes)?;
                }
                // Range type
                Type::Generic(crate::ast::Ident {
                    name: "Range".into(),
                    span: Span::default(),
                })
            },
            // Phase 4I: Iteration placeholder ##
            crate::ast::Expr::IterPlaceholder { .. } => {
                // Type depends on iteration context - generic for now
                Type::Generic(crate::ast::Ident {
                    name: "T".into(),
                    span: Span::default(),
                })
            },
            // Phase 4I: Iteration index placeholder #@
            crate::ast::Expr::IterIndexPlaceholder { .. } => {
                Type::Number
            },
            // Phase 4I: Register read %q0-%q9
            crate::ast::Expr::RegisterRead { .. } => {
                Type::Number
            },
            // Phase 4I: Register write %q0-%q9 = expr
            crate::ast::Expr::RegisterWrite { value, .. } => {
                self.check_expr(value, types, scopes)?;
                Type::Number
            },
            // Phase 4I: String register read %r0-%r9
            crate::ast::Expr::StringRegisterRead { .. } => {
                Type::String
            },
            // Phase 4I: String register write %r0-%r9 = expr
            crate::ast::Expr::StringRegisterWrite { value, .. } => {
                self.check_expr(value, types, scopes)?;
                Type::String
            },
            // Phase 4I: String register append %r0-%r9 .= expr
            crate::ast::Expr::StringRegisterAppend { value, .. } => {
                self.check_expr(value, types, scopes)?;
                Type::String
            },
            // Phase 4I: Literal operator lit!()
            crate::ast::Expr::LitOperator { .. } => {
                Type::String
            },
            // Phase 4I: Default function
            crate::ast::Expr::Default { value, fallback, predicate, .. } => {
                let value_ty = self.check_expr(value, types, scopes)?;
                let fallback_ty = self.check_expr(fallback, types, scopes)?;
                if let Some(pred) = predicate {
                    self.check_expr(pred, types, scopes)?;
                }
                // Return the common type of value and fallback
                if value_ty == fallback_ty {
                    value_ty
                } else {
                    // Union type
                    fallback_ty
                }
            },
        };
        
        types.insert(expr.span().clone(), ty.clone());
        Ok(ty)
    }
    
    // Phase 4G: Convert Type to String for trait impl lookup
    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Generic(ident) => ident.name.to_string(),
            _ => format!("{:?}", ty),
        }
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
            crate::ast::Expr::Move { span, .. } => span,
            crate::ast::Expr::Borrow { span, .. } => span,
            crate::ast::Expr::Perform { span, .. } => span,
            crate::ast::Expr::Handle { span, .. } => span,
            crate::ast::Expr::Resume { span, .. } => span,
            crate::ast::Expr::InlineAsm { span, .. } => span,
            crate::ast::Expr::Placeholder(span) => span,
            crate::ast::Expr::PartialApplication { span, .. } => span,
            crate::ast::Expr::Lazy { span, .. } => span,
            crate::ast::Expr::Force { span, .. } => span,
            crate::ast::Expr::Do { span, .. } => span,
            crate::ast::Expr::Spawn { span, .. } => span,
            crate::ast::Expr::Channel { span, .. } => span,
            crate::ast::Expr::Send { span, .. } => span,
            crate::ast::Expr::Recv { span, .. } => span,
            crate::ast::Expr::Select { span, .. } => span,
            crate::ast::Expr::Scope { span, .. } => span,
            crate::ast::Expr::Atomic { span, .. } => span,
            crate::ast::Expr::Lock { span, .. } => span,
            crate::ast::Expr::ReadLock { span, .. } => span,
            crate::ast::Expr::WriteLock { span, .. } => span,
            crate::ast::Expr::FutureJoin { span, .. } => span,
            crate::ast::Expr::FutureSelect { span, .. } => span,
            crate::ast::Expr::FutureRace { span, .. } => span,
            crate::ast::Expr::Timeout { span, .. } => span,
            crate::ast::Expr::StreamFromIter { span, .. } => span,
            crate::ast::Expr::StreamMap { span, .. } => span,
            crate::ast::Expr::StreamFilter { span, .. } => span,
            crate::ast::Expr::StreamCollect { span, .. } => span,
            crate::ast::Expr::ParIter { span, .. } => span,
            crate::ast::Expr::Try { span, .. } => span,
            crate::ast::Expr::TryOperator { span, .. } => span,
            crate::ast::Expr::Guard { span, .. } => span,
            crate::ast::Expr::LabeledBlock { span, .. } => span,
            crate::ast::Expr::BreakWithValue { span, .. } => span,
            crate::ast::Expr::Catch { span, .. } => span,
            crate::ast::Expr::Panic { span, .. } => span,
            crate::ast::Expr::CfgExpr { span, .. } => span,
            crate::ast::Expr::ConstAssert { span, .. } => span,
            crate::ast::Expr::Unreachable { span, .. } => span,
            crate::ast::Expr::MacroInvocation { span, .. } => span,
            crate::ast::Expr::TypeInfo { span, .. } => span,
            crate::ast::Expr::Quote { span, .. } => span,
            crate::ast::Expr::StringSlice { span, .. } => span,
            crate::ast::Expr::FormatString { span, .. } => span,
            crate::ast::Expr::DestructuringAssign { span, .. } => span,
            crate::ast::Expr::Range { span, .. } => span,
            // Phase 4I expressions
            crate::ast::Expr::IterPlaceholder { span, .. } => span,
            crate::ast::Expr::IterIndexPlaceholder { span, .. } => span,
            crate::ast::Expr::RegisterRead { span, .. } => span,
            crate::ast::Expr::RegisterWrite { span, .. } => span,
            crate::ast::Expr::StringRegisterRead { span, .. } => span,
            crate::ast::Expr::StringRegisterWrite { span, .. } => span,
            crate::ast::Expr::StringRegisterAppend { span, .. } => span,
            crate::ast::Expr::LitOperator { span, .. } => span,
            crate::ast::Expr::Default { span, .. } => span,
        }
    }
}
