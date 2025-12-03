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
    /// The value is currently borrowed immutably. Multiple immutable borrows are allowed.
    ImmutablyBorrowed(Vec<Span>),
    /// The value is currently borrowed mutably. Only one mutable borrow is allowed.
    MutablyBorrowed(Span),
}

/// Phase 4A: Borrow type for tracking references
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowType {
    /// Immutable borrow (&T)
    Immutable,
    /// Mutable borrow (&mut T)
    Mutable,
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
    /// Phase 4A: Track active borrows for conflict detection
    active_borrows: HashMap<String, Vec<(BorrowType, Span)>>,
}

impl BorrowChecker {
    /// Creates a new borrow checker with an empty scope stack.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
            active_borrows: HashMap::new(),
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
        for (pattern, _, _) in &func.params {
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
            ast::Stmt::Break { value, .. } => {
                // Phase 4E: Break statement
                if let Some(v) = value {
                    self.check_expr(v);
                }
            }
            ast::Stmt::Continue { .. } => {
                // Phase 4E: Continue statement (no ownership concerns)
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
            ast::Expr::Index { expr, index, .. } => {
                self.check_expr(expr);
                self.check_expr(index);
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
            ast::Expr::Move { expr, span } => {
                // Phase 4A: Explicit move - mark the value as moved
                if let ast::Expr::Ident(ident) = expr.as_ref() {
                    self.mark_moved(&ident.name, span);
                }
                self.check_expr(expr);
            }
            ast::Expr::Borrow { expr, mutable, span } => {
                // Phase 4A: Borrow expression - check borrow rules
                if let ast::Expr::Ident(ident) = expr.as_ref() {
                    let borrow_type = if *mutable {
                        BorrowType::Mutable
                    } else {
                        BorrowType::Immutable
                    };
                    self.check_borrow(&ident.name, borrow_type, span);
                }
                self.check_expr(expr);
            }
            ast::Expr::Perform { args, .. } => {
                // Phase 4A: Perform effect - check all arguments
                for arg in args {
                    self.check_expr(arg);
                }
            }
            ast::Expr::Handle { body, handlers, .. } => {
                // Phase 4A: Handle effects - check body and handlers
                self.check_block(body);
                for handler in handlers {
                    self.check_block(&handler.body);
                }
            }
            ast::Expr::Resume { value, .. } => {
                // Phase 4A: Resume - check the value
                self.check_expr(value);
            }
            ast::Expr::InlineAsm { .. } => {
                // Phase 4A: Inline assembly - no ownership checks needed
            }
            ast::Expr::Placeholder(_) => {
                // Phase 4C: Placeholder - no ownership checks needed
            }
            ast::Expr::PartialApplication { func, args, .. } => {
                // Phase 4C: Partial application - check function and fixed arguments
                self.check_expr(func);
                for arg in args {
                    if let ast::PartialArg::Fixed(expr) = arg {
                        self.check_expr(expr);
                    }
                }
            }
            ast::Expr::Lazy { expr, .. } => {
                // Phase 4C: Lazy evaluation - check inner expression
                self.check_expr(expr);
            }
            ast::Expr::Force { expr, .. } => {
                // Phase 4C: Force evaluation - check inner expression
                self.check_expr(expr);
            }
            ast::Expr::Do { bindings, result, .. } => {
                // Phase 4C: Monadic do-notation
                for binding in bindings {
                    self.check_expr(&binding.expr);
                }
                self.check_expr(result);
            }
            ast::Expr::Spawn { body, .. } => {
                // Phase 4D: Spawn - check body
                self.check_block(body);
            }
            ast::Expr::Channel { buffer_size, .. } => {
                // Phase 4D: Channel
                if let Some(size) = buffer_size {
                    self.check_expr(size);
                }
            }
            ast::Expr::Send { channel, value, .. } => {
                // Phase 4D: Send
                self.check_expr(channel);
                self.check_expr(value);
            }
            ast::Expr::Recv { channel, .. } => {
                // Phase 4D: Recv
                self.check_expr(channel);
            }
            ast::Expr::Select { arms, .. } => {
                // Phase 4D: Select
                for arm in arms {
                    self.check_block(&arm.body);
                }
            }
            ast::Expr::Scope { body, .. } => {
                // Phase 4D: Scope
                self.check_block(body);
            }
            ast::Expr::Atomic { target, value, .. } => {
                // Phase 4D: Atomic
                self.check_expr(target);
                if let Some(v) = value {
                    self.check_expr(v);
                }
            }
            ast::Expr::Lock { mutex, .. } => {
                // Phase 4D: Lock
                self.check_expr(mutex);
            }
            ast::Expr::ReadLock { rwlock, .. } => {
                // Phase 4D: ReadLock
                self.check_expr(rwlock);
            }
            ast::Expr::WriteLock { rwlock, .. } => {
                // Phase 4D: WriteLock
                self.check_expr(rwlock);
            }
            ast::Expr::FutureJoin { futures, .. } => {
                // Phase 4D: Future::join
                for future in futures {
                    self.check_expr(future);
                }
            }
            ast::Expr::FutureSelect { futures, .. } => {
                // Phase 4D: Future::select
                for future in futures {
                    self.check_expr(future);
                }
            }
            ast::Expr::FutureRace { futures, .. } => {
                // Phase 4D: Future::race
                for future in futures {
                    self.check_expr(future);
                }
            }
            ast::Expr::Timeout { duration, .. } => {
                // Phase 4D: Timeout
                self.check_expr(duration);
            }
            ast::Expr::StreamFromIter { iter, .. } => {
                // Phase 4D: Stream::from_iter
                self.check_expr(iter);
            }
            ast::Expr::StreamMap { stream, mapper, .. } => {
                // Phase 4D: Stream map
                self.check_expr(stream);
                self.check_expr(mapper);
            }
            ast::Expr::StreamFilter { stream, predicate, .. } => {
                // Phase 4D: Stream filter
                self.check_expr(stream);
                self.check_expr(predicate);
            }
            ast::Expr::StreamCollect { stream, .. } => {
                // Phase 4D: Stream collect
                self.check_expr(stream);
            }
            ast::Expr::ParIter { collection, .. } => {
                // Phase 4D: par_iter
                self.check_expr(collection);
            }
            ast::Expr::Try { body, catch_clauses, .. } => {
                // Phase 4E: Try-catch
                self.check_block(body);
                for clause in catch_clauses {
                    self.check_block(&clause.body);
                }
            }
            ast::Expr::TryOperator { expr, .. } => {
                // Phase 4E: ? operator
                self.check_expr(expr);
            }
            ast::Expr::Guard { condition, else_block, .. } => {
                // Phase 4E: Guard clause
                self.check_expr(condition);
                self.check_block(else_block);
            }
            ast::Expr::LabeledBlock { block, .. } => {
                // Phase 4E: Labelled block
                self.check_block(block);
            }
            ast::Expr::BreakWithValue { value, .. } => {
                // Phase 4E: Break with value
                if let Some(v) = value {
                    self.check_expr(v);
                }
            }
            ast::Expr::Catch { expr, handler, .. } => {
                // Phase 4E: Catch expression
                self.check_expr(expr);
                self.check_expr(handler);
            }
            ast::Expr::Panic { message, .. } => {
                // Phase 4E: Panic
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }
            ast::Expr::CfgExpr { then_expr, else_expr, .. } => {
                // Phase 4E: Conditional compilation
                self.check_expr(then_expr);
                if let Some(else_e) = else_expr {
                    self.check_expr(else_e);
                }
            }
            ast::Expr::ConstAssert { condition, message, .. } => {
                // Phase 4E: Const assertion
                self.check_expr(condition);
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }
            ast::Expr::Unreachable { message, .. } => {
                // Phase 4E: Unreachable
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }
            ast::Expr::MacroInvocation { args, .. } => {
                // Phase 4F: Macro invocation
                for arg in args {
                    self.check_expr(arg);
                }
            }
            ast::Expr::TypeInfo { .. } => {
                // Phase 4F: Type reflection (no ownership concerns)
            }
            ast::Expr::Quote { code, .. } => {
                // Phase 4F: Quote
                self.check_block(code);
            }
            // Phase 4G: String slicing
            ast::Expr::StringSlice { string, range, .. } => {
                self.check_expr(string);
                if let Some(start) = &range.start {
                    self.check_expr(start);
                }
                if let Some(end) = &range.end {
                    self.check_expr(end);
                }
                if let Some(step) = &range.step {
                    self.check_expr(step);
                }
            }
            // Phase 4G: Format strings
            ast::Expr::FormatString { parts, .. } => {
                for part in parts {
                    if let ast::FormatPart::Formatted { expr, .. } = part {
                        self.check_expr(expr);
                    }
                }
            }
            // Phase 4G: Destructuring assignment
            ast::Expr::DestructuringAssign { value, .. } => {
                self.check_expr(value);
            }
            // Phase 4G: Range expression
            ast::Expr::Range { start, end, step, .. } => {
                if let Some(s) = start {
                    self.check_expr(s);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
                if let Some(st) = step {
                    self.check_expr(st);
                }
            }
            ast::Expr::Literal(_, _) => {
                // Literals don't involve ownership
            }
            // Phase 4I: Iteration placeholders
            ast::Expr::IterPlaceholder { .. } => {
                // Resolved at compile time
            }
            ast::Expr::IterIndexPlaceholder { .. } => {
                // Resolved at compile time
            }
            // Phase 4I: Register variables
            ast::Expr::RegisterRead { .. } => {
                // Registers are always valid
            }
            ast::Expr::RegisterWrite { value, .. } => {
                self.check_expr(value);
            }
            ast::Expr::StringRegisterRead { .. } => {
                // String registers are always valid
            }
            ast::Expr::StringRegisterWrite { value, .. } => {
                self.check_expr(value);
            }
            ast::Expr::StringRegisterAppend { value, .. } => {
                self.check_expr(value);
            }
            // Phase 4I: Literal operator
            ast::Expr::LitOperator { .. } => {
                // No ownership concerns
            }
            // Phase 4I: Default function
            ast::Expr::Default { value, fallback, predicate, .. } => {
                self.check_expr(value);
                self.check_expr(fallback);
                if let Some(pred) = predicate {
                    self.check_expr(pred);
                }
            }
        }
    }
    
    /// Marks a variable as explicitly moved.
    fn mark_moved(&mut self, name: &str, span: &Span) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get_mut(name) {
                *state = OwnershipState::Moved(span.clone());
                return;
            }
        }
    }

    /// Phase 4A: Checks if a borrow is allowed and records it
    fn check_borrow(&mut self, name: &str, borrow_type: BorrowType, span: &Span) {
        // Check current ownership state
        for scope in self.scopes.iter().rev() {
            if let Some(state) = scope.get(name) {
                match state {
                    OwnershipState::Moved(moved_span) => {
                        self.errors.push(format!(
                            "Cannot borrow moved value '{}' at byte offset {}. Value was moved at byte offset {}.",
                            name, span.start, moved_span.start
                        ));
                        return;
                    }
                    OwnershipState::MutablyBorrowed(borrow_span) => {
                        self.errors.push(format!(
                            "Cannot borrow '{}' at byte offset {} because it is already mutably borrowed at byte offset {}.",
                            name, span.start, borrow_span.start
                        ));
                        return;
                    }
                    OwnershipState::ImmutablyBorrowed(borrows) => {
                        if borrow_type == BorrowType::Mutable {
                            self.errors.push(format!(
                                "Cannot mutably borrow '{}' at byte offset {} because it is already immutably borrowed at byte offset {}.",
                                name, span.start, borrows[0].start
                            ));
                            return;
                        }
                        // Multiple immutable borrows are OK
                    }
                    OwnershipState::Owned => {
                        // Borrow is allowed
                    }
                }
                break;
            }
        }

        // Record the borrow
        self.active_borrows
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push((borrow_type, span.clone()));

        // Update ownership state
        for scope in self.scopes.iter_mut().rev() {
            if let Some(state) = scope.get_mut(name) {
                match borrow_type {
                    BorrowType::Immutable => {
                        match state {
                            OwnershipState::ImmutablyBorrowed(borrows) => {
                                borrows.push(span.clone());
                            }
                            _ => {
                                *state = OwnershipState::ImmutablyBorrowed(vec![span.clone()]);
                            }
                        }
                    }
                    BorrowType::Mutable => {
                        *state = OwnershipState::MutablyBorrowed(span.clone());
                    }
                }
                return;
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
                    OwnershipState::ImmutablyBorrowed(_) | OwnershipState::MutablyBorrowed(_) => {
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
