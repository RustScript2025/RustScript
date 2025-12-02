//! Abstract Syntax Tree definitions for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module defines the complete AST structure for the RustScript language.
//! The AST is designed to be straightforward to traverse and transform, with
//! comprehensive span information for error reporting and source mapping.
//! 
//! All AST nodes are serialisable for caching and debugging purposes.

use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::path::PathBuf;
use std::hash::{Hash, Hasher};

/// Represents a location in source code.
/// 
/// Spans are used throughout the AST to track where each construct originated
/// in the source file. This enables detailed error messages and accurate source
/// map generation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Span {
    /// Byte offset where this span starts in the source file.
    pub start: usize,
    /// Byte offset where this span ends (exclusive).
    pub end: usize,
    /// Which source file this span refers to.
    pub file_id: usize,
}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
        self.file_id.hash(state);
    }
}

/// An identifier (variable name, function name, etc.).
/// 
/// Identifiers use reference-counted strings to avoid redundant allocations
/// when the same name appears multiple times.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Ident {
    /// The identifier's name.
    #[serde(with = "rc_str_serde")]
    pub name: Rc<str>,
    /// Where this identifier appears in the source.
    pub span: Span,
}

/// Custom serde module for Rc<str> serialisation
mod rc_str_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::rc::Rc;

    pub fn serialize<S>(value: &Rc<str>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        value.as_ref().serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Rc<str>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(s.into())
    }
}

/// Phase 4A: Lifetime annotation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Lifetime {
    #[serde(with = "rc_str_serde")]
    pub name: Rc<str>,
    pub span: Span,
}

/// Type annotations in the RustScript type system.
/// 
/// RustScript supports gradual typing, allowing types to be explicitly
/// annotated or inferred. The type system includes primitives, compound
/// types, and generics.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Type {
    /// Floating-point number (f64 in WebAssembly).
    Number,
    /// UTF-8 encoded string.
    String,
    /// Boolean value (true or false).
    Boolean,
    /// Array containing elements of a single type.
    Array(Box<Type>),
    /// Tuple with a fixed number of heterogeneous elements.
    Tuple(Vec<Type>),
    /// Record with named fields (similar to structs).
    Record(Vec<(Ident, Type)>),
    /// Function type with parameters and return type.
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    /// Generic type parameter or user-defined type.
    Generic(Ident),
    /// Type to be inferred by the type checker.
    Infer,
    /// Phase 4A: Reference type with optional lifetime
    Reference {
        inner: Box<Type>,
        mutable: bool,
        lifetime: Option<Lifetime>,
    },
    /// Phase 4A: Array with const generic size
    ConstArray {
        element_type: Box<Type>,
        size: ConstExpr,
    },
    /// Phase 4B: Union type (A | B | C)
    Union(Vec<Type>),
    /// Phase 4B: Intersection type (A & B & C)
    Intersection(Vec<Type>),
    /// Phase 4B: Higher-kinded type (type constructor)
    /// Example: F<_> where F is a type constructor like Option or Vec
    HigherKinded {
        constructor: Ident,
        /// Number of type parameters this constructor takes
        arity: usize,
    },
    /// Phase 4B: Applied higher-kinded type
    /// Example: F<i32> where F is a higher-kinded type
    AppliedHigherKinded {
        constructor: Box<Type>,
        args: Vec<Type>,
    },
    /// Phase 4B: Phantom type marker
    /// PhantomData<T> - zero-sized type that acts like it owns T
    PhantomData(Box<Type>),
    /// Phase 4B: Refinement type with predicate
    /// Example: {x: i32 | x > 0} for positive integers
    Refinement {
        base: Box<Type>,
        binder: Ident,
        predicate: Box<Expr>,
    },
    /// Phase 4B: Dependent type (type depends on value)
    /// Example: Vec<n> where n is a runtime value
    Dependent {
        constructor: Ident,
        /// Value parameters that the type depends on
        value_params: Vec<Expr>,
    },
    /// Phase 4B: Type-level function application
    /// Example: Add<N, M> computes N + M at type level
    TypeLevelApp {
        func: Ident,
        args: Vec<Type>,
    },
    /// Phase 4B: Type-level literal (for type computations)
    TypeLevelLit(i32),
    /// Phase 4B: Existential type (hides concrete type)
    /// Example: impl Trait or exists T. Trait<T>
    Existential {
        /// Trait bounds that the hidden type must satisfy
        bounds: Vec<Ident>,
    },
    /// Phase 4B: GADT constructor return type
    /// Example: Some<T> in enum Option<T> where Some returns Option<T>
    GADTReturn {
        constructor: Ident,
        type_args: Vec<Type>,
    },
    /// Phase 4C: Immutable vector (persistent data structure)
    ImmutableVec(Box<Type>),
    /// Phase 4C: Immutable map (persistent data structure)
    ImmutableMap {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },
    /// Phase 4C: Immutable set (persistent data structure)
    ImmutableSet(Box<Type>),
}

/// Phase 4A: Const expression for compile-time evaluation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConstExpr {
    /// Literal number constant
    Number(f64),
    /// Named constant parameter
    Param(Ident),
    /// Binary operation on const expressions
    Binary {
        left: Box<ConstExpr>,
        op: BinaryOp,
        right: Box<ConstExpr>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Pattern {
    Ident(Ident),
    Tuple(Vec<Pattern>),
    Record(Vec<(Ident, Pattern)>),
    Literal(Literal),
    Wildcard(Span),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Literal {
    Number(f64),
    #[serde(with = "rc_str_serde")]
    String(Rc<str>),
    /// Interpolated string like "Hello, {name}!".
    InterpolatedString(Vec<StringPart>),
    Boolean(bool),
    Array(Vec<Expr>),
    Record(Vec<(Ident, Expr)>),
    Tuple(Vec<Expr>),
    #[serde(with = "rc_str_serde")]
    Regex(Rc<str>),
}

/// A component of an interpolated string.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum StringPart {
    /// Static text portion.
    #[serde(with = "rc_str_serde")]
    Text(Rc<str>),
    /// Expression to evaluate and convert to a string.
    Expr(Box<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expr {
    Literal(Literal, Span),
    Ident(Ident),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<(Option<Ident>, Expr)>,
        span: Span,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Pipeline {
        steps: Vec<Expr>,
        span: Span,
    },
    Async(Box<Expr>, Span),
    Await(Box<Expr>, Span),
    Block(Block),
    StructInit {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
        span: Span,
    },
    FieldAccess {
        expr: Box<Expr>,
        field: Ident,
        span: Span,
    },
    /// Array/tuple indexing: expr[index]
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    /// Optional chaining operator: expr?.field
    OptionalChain {
        expr: Box<Expr>,
        field: Ident,
        span: Span,
    },
    /// Null coalescing operator: expr ?? default
    NullCoalesce {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    /// List comprehension: [expr for pattern in iter if condition]
    ListComprehension {
        expr: Box<Expr>,
        pattern: Pattern,
        iter: Box<Expr>,
        condition: Option<Box<Expr>>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
        span: Span,
    },
    Loop {
        body: Block,
        span: Span,
    },
    While {
        condition: Box<Expr>,
        body: Block,
        span: Span,
    },
    /// Generator yield expression.
    Yield {
        value: Option<Box<Expr>>,
        span: Span,
    },
    /// Compile-time evaluation block: comptime { ... }
    Comptime {
        block: Block,
        span: Span,
    },
    /// Design by Contract assertion: requires/ensures
    ContractExpr {
        kind: ContractKind,
        condition: Box<Expr>,
        #[serde(with = "option_rc_str_serde")]
        message: Option<Rc<str>>,
        span: Span,
    },
    /// Phase 4A: Explicit move expression
    Move {
        expr: Box<Expr>,
        span: Span,
    },
    /// Phase 4A: Borrow expression - creates a reference (&expr or &mut expr)
    Borrow {
        expr: Box<Expr>,
        mutable: bool,
        span: Span,
    },
    /// Phase 4A: Perform an effect
    Perform {
        effect: Ident,
        args: Vec<Expr>,
        span: Span,
    },
    /// Phase 4A: Handle effects with handlers
    Handle {
        body: Block,
        handlers: Vec<EffectHandler>,
        span: Span,
    },
    /// Phase 4A: Resume from an effect handler
    Resume {
        value: Box<Expr>,
        span: Span,
    },
    /// Phase 4A: Inline WebAssembly assembly
    InlineAsm {
        instructions: Vec<WasmInstruction>,
        span: Span,
    },
    /// Phase 4C: Placeholder for partial application (_)
    Placeholder(Span),
    /// Phase 4C: Partial application result
    PartialApplication {
        func: Box<Expr>,
        args: Vec<PartialArg>,
        span: Span,
    },
    /// Phase 4C: Lazy evaluation - deferred computation
    Lazy {
        expr: Box<Expr>,
        span: Span,
    },
    /// Phase 4C: Force evaluation of lazy expression
    Force {
        expr: Box<Expr>,
        span: Span,
    },
    /// Phase 4C: Monadic do-notation block
    Do {
        bindings: Vec<DoBinding>,
        result: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Spawn a new thread/task
    Spawn {
        body: Block,
        span: Span,
    },
    /// Phase 4D: Create a channel for message passing
    Channel {
        buffer_size: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4D: Send a message through a channel
    Send {
        channel: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Receive a message from a channel
    Recv {
        channel: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Select over multiple channel operations
    Select {
        arms: Vec<SelectArm>,
        span: Span,
    },
    /// Phase 4D: Scoped thread spawning
    Scope {
        body: Block,
        span: Span,
    },
    /// Phase 4D: Atomic operation
    Atomic {
        operation: AtomicOp,
        target: Box<Expr>,
        value: Option<Box<Expr>>,
        ordering: MemoryOrdering,
        span: Span,
    },
    /// Phase 4D: Mutex lock
    Lock {
        mutex: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: RwLock read
    ReadLock {
        rwlock: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: RwLock write
    WriteLock {
        rwlock: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Future::join - wait for multiple futures
    FutureJoin {
        futures: Vec<Expr>,
        span: Span,
    },
    /// Phase 4D: Future::select - race between futures
    FutureSelect {
        futures: Vec<Expr>,
        span: Span,
    },
    /// Phase 4D: Future::race - first future to complete
    FutureRace {
        futures: Vec<Expr>,
        span: Span,
    },
    /// Phase 4D: Timeout wrapper
    Timeout {
        duration: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Stream creation from iterator
    StreamFromIter {
        iter: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Stream map operation
    StreamMap {
        stream: Box<Expr>,
        mapper: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Stream filter operation
    StreamFilter {
        stream: Box<Expr>,
        predicate: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Stream collect operation
    StreamCollect {
        stream: Box<Expr>,
        span: Span,
    },
    /// Phase 4D: Parallel iterator creation
    ParIter {
        collection: Box<Expr>,
        span: Span,
    },
    /// Phase 4E: Try block with catch handlers
    Try {
        body: Block,
        catch_clauses: Vec<CatchClause>,
        span: Span,
    },
    /// Phase 4E: Question mark operator for error propagation
    TryOperator {
        expr: Box<Expr>,
        span: Span,
    },
    /// Phase 4E: Guard clause with early return
    Guard {
        condition: Box<Expr>,
        else_block: Block,
        span: Span,
    },
    /// Phase 4E: Labelled block
    LabeledBlock {
        label: Ident,
        block: Block,
        span: Span,
    },
    /// Phase 4E: Break with value and optional label
    BreakWithValue {
        label: Option<Ident>,
        value: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4E: Catch expression for inline error handling
    Catch {
        expr: Box<Expr>,
        handler: Box<Expr>,
        span: Span,
    },
    /// Phase 4E: Panic with message
    Panic {
        message: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4E: Conditional compilation expression
    CfgExpr {
        condition: CfgCondition,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4E: Const assertion
    ConstAssert {
        condition: Box<Expr>,
        message: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4E: Unreachable code marker
    Unreachable {
        message: Option<Box<Expr>>,
        span: Span,
    },
    /// Phase 4F: Macro invocation
    MacroInvocation {
        name: Ident,
        args: Vec<Expr>,
        span: Span,
    },
    /// Phase 4F: Compile-time reflection
    TypeInfo {
        type_expr: Box<Type>,
        span: Span,
    },
    /// Phase 4F: Quote expression for code generation
    Quote {
        code: Block,
        span: Span,
    },
    /// Phase 4G: String slice with range
    StringSlice {
        string: Box<Expr>,
        range: SliceRange,
        span: Span,
    },
    /// Phase 4G: Format string with advanced formatting
    FormatString {
        parts: Vec<FormatPart>,
        span: Span,
    },
    /// Phase 4G: Destructuring assignment
    DestructuringAssign {
        pattern: Pattern,
        value: Box<Expr>,
        span: Span,
    },
    /// Phase 4G: Range expression (start..end or start..end::step)
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
        span: Span,
    },
}

/// Phase 4F: Macro definition
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct MacroDefinition {
    pub name: Ident,
    pub rules: Vec<MacroRule>,
    pub span: Span,
}

/// Phase 4F: Macro rule (pattern => expansion)
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct MacroRule {
    pub pattern: Vec<MacroToken>,
    pub expansion: Vec<MacroToken>,
}

/// Phase 4F: Macro token
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum MacroToken {
    Literal(String),
    Variable(Ident),
    Repetition(Vec<MacroToken>),
}

/// Phase 4F: Derive attribute for procedural macros
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DeriveAttribute {
    pub traits: Vec<Ident>,
    pub span: Span,
}

/// Phase 4F: Custom attribute
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CustomAttribute {
    pub name: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Phase 4G: Slice range for string/array slicing
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SliceRange {
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub step: Option<Box<Expr>>,
}

/// Phase 4G: Format string part
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FormatPart {
    #[serde(with = "rc_str_serde")]
    Text(Rc<str>),
    Formatted {
        expr: Box<Expr>,
        format_spec: Option<FormatSpec>,
    },
}

/// Phase 4G: Format specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FormatSpec {
    #[serde(with = "option_rc_str_serde")]
    pub fill: Option<Rc<str>>,
    pub align: Option<Alignment>,
    pub sign: Option<Sign>,
    pub width: Option<usize>,
    pub precision: Option<usize>,
    #[serde(with = "option_rc_str_serde")]
    pub type_spec: Option<Rc<str>>,
}

/// Phase 4G: Alignment for format strings
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Alignment {
    Left,
    Right,
    Center,
}

/// Phase 4G: Sign for format strings
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Sign {
    Plus,
    Minus,
    Space,
}

/// Phase 4E: Conditional compilation condition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum CfgCondition {
    Target(String),
    Feature(String),
    Debug,
    Release,
    Test,
}

/// Phase 4E: Catch clause for try blocks
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CatchClause {
    pub error_type: Option<Ident>,
    pub binding: Option<Ident>,
    pub body: Block,
}

/// Phase 4C: Binding in do-notation (let x <- expr)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DoBinding {
    pub pattern: Pattern,
    pub expr: Expr,
}

/// Phase 4C: Argument in partial application (either fixed or placeholder)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PartialArg {
    /// Fixed argument value
    Fixed(Expr),
    /// Placeholder (_) to be filled later
    Placeholder,
}

/// Phase 4A: WebAssembly instruction for inline assembly
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum WasmInstruction {
    /// Local variable operations
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    
    /// Stack operations
    Drop,
    Select,
    
    /// Constants
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    
    /// Arithmetic operations
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    
    /// Comparison operations
    I32Eq,
    I32Ne,
    I32LtS,
    I32GtS,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    
    /// Memory operations
    I32Load,
    I32Store,
    F64Load,
    F64Store,
    
    /// Control flow
    Call(u32),
    Return,
    
    /// Custom instruction with raw string
    Raw(String),
}

/// Phase 4A: Effect handler for algebraic effects
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EffectHandler {
    pub effect: Ident,
    pub params: Vec<Pattern>,
    pub body: Block,
    pub span: Span,
}

/// Custom serde module for Option<Rc<str>> serialisation
mod option_rc_str_serde {
    use serde::{Deserialize, Deserializer, Serializer};
    use std::rc::Rc;

    pub fn serialize<S>(value: &Option<Rc<str>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match value {
            Some(s) => serializer.serialize_some(s.as_ref()),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Rc<str>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let opt: Option<String> = Option::deserialize(deserializer)?;
        Ok(opt.map(|s| s.into()))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div,
    Eq, Neq, Lt, Gt, Leq, Geq,
    And, Or,
    Pipeline,
    /// Phase 4C: Forward function composition (f >> g)
    ComposeForward,
    /// Phase 4C: Backward function composition (f << g)
    ComposeBackward,
    /// Phase 4C: Applicative fmap (<$>)
    ApplicativeFmap,
    /// Phase 4C: Applicative apply (<*>)
    ApplicativeApply,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Stmt {
    Let {
        mutable: bool,
        pattern: Pattern,
        type_ann: Option<Type>,
        value: Option<Expr>,
        span: Span,
    },
    Expr(Expr, Span),
    Return(Option<Expr>, Span),
    Guard {
        condition: Expr,
        else_block: Block,
        span: Span,
    },
    Defer {
        block: Block,
        span: Span,
    },
    /// Phase 4E: Break with optional label and value
    Break {
        label: Option<Ident>,
        value: Option<Expr>,
        span: Span,
    },
    /// Phase 4E: Continue with optional label
    Continue {
        label: Option<Ident>,
        span: Span,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Pattern, Option<Type>, Option<Expr>)>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub async_: bool,
    /// Whether this is a generator function (uses yield).
    pub generator: bool,
    /// Guard condition for pattern-matched dispatch.
    pub guard: Option<Expr>,
    /// Design by Contract specifications.
    pub contracts: Vec<Contract>,
    /// Effect annotations (I/O, state mutation, purity, etc.).
    pub effects: Vec<Effect>,
    /// Phase 4A: Lifetime parameters
    pub lifetimes: Vec<Lifetime>,
    /// Phase 4A: Const generic parameters
    pub const_params: Vec<Ident>,
    /// Phase 4A: Enable tail call optimisation
    pub tail_call_optimized: bool,
    /// Phase 4B: Where clause constraints
    pub where_clause: Vec<WhereClause>,
    /// Phase 4C: Curried parameter groups (for currying)
    pub param_groups: Vec<Vec<(Pattern, Option<Type>, Option<Expr>)>>,
    /// Phase 4C: Enable memoisation (caching of results)
    pub memoized: bool,
    /// Phase 4H: Const function (compile-time evaluation)
    pub const_fn: bool,
    pub span: Span,
}

/// Phase 4B: Where clause for complex type bounds
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct WhereClause {
    pub type_param: Ident,
    pub bounds: Vec<TypeBound>,
}

/// Phase 4B: Type bound (trait bound or lifetime bound)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TypeBound {
    /// Trait bound (T: Display)
    Trait(Ident),
    /// Lifetime bound (T: 'a)
    Lifetime(Lifetime),
    /// Equality bound (T = ConcreteType)
    Equality(Type),
}

/// Design by Contract specification.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Contract {
    pub kind: ContractKind,
    pub condition: Expr,
    #[serde(with = "option_rc_str_serde")]
    pub message: Option<Rc<str>>,
    pub span: Span,
}

/// Types of Design by Contract assertions.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ContractKind {
    /// Precondition (requires clause).
    Requires,
    /// Postcondition (ensures clause).
    Ensures,
    /// Loop or class invariant (always true).
    Invariant,
}

/// Effect system annotations for tracking side effects.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Effect {
    /// Pure function with no side effects.
    Pure,
    /// Performs I/O operations.
    IO,
    /// Mutates state.
    State,
    /// May panic or throw errors.
    Throws,
    /// Allocates heap memory.
    Alloc,
    /// Reads from external sources.
    Read,
    /// Writes to external destinations.
    Write,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<Item>,
    pub span: Span,
    pub file_path: PathBuf,
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Item {
    Function(Function),
    /// Multiple function definitions with the same name (multiple dispatch).
    MultiFn {
        name: Ident,
        variants: Vec<Function>,
        span: Span,
    },
    Struct(Struct),
    Import(Import),
    Extend {
        target: Type,
        methods: Vec<Function>,
        span: Span,
    },
    /// Compile-time evaluation block (Zig-style).
    Comptime {
        block: Block,
        span: Span,
    },
    /// Phase 4A: Trait definition
    Trait(Trait),
    /// Phase 4A: Trait implementation
    TraitImpl(TraitImpl),
    /// Phase 4B: Type alias
    TypeAlias(TypeAlias),
    /// Phase 4B: Type-level function
    TypeFunction(TypeFunction),
    /// Phase 4B: GADT (Generalised Algebraic Data Type)
    Enum(Enum),
}

/// Phase 4B: Type-level function for type computations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypeFunction {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Type,
    pub span: Span,
}

/// Phase 4B: GADT (Generalised Algebraic Data Type)
/// Allows constructors to have different return types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Enum {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

/// Phase 4B: GADT variant with optional explicit return type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    /// Fields for this variant (can be named or unnamed)
    pub fields: VariantFields,
    /// Optional explicit return type (for GADTs)
    /// Example: Some(T) -> Option<T> vs None -> Option<Never>
    pub return_type: Option<Type>,
    pub span: Span,
}

/// Fields in an enum variant
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum VariantFields {
    /// No fields (unit variant)
    Unit,
    /// Unnamed fields (tuple variant)
    Tuple(Vec<Type>),
    /// Named fields (struct variant)
    Named(Vec<(Ident, Type)>),
}

/// Phase 4B: Type alias definition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypeAlias {
    pub name: Ident,
    pub type_params: Vec<Ident>,
    pub target: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>,
    /// Phase 4A: Const generic parameters
    pub const_params: Vec<Ident>,
    /// Phase 4B: Tuple struct (newtype pattern) - unnamed fields
    pub tuple_fields: Vec<Type>,
    /// Phase 4B: Phantom type parameters (exist only at compile-time)
    pub phantom_params: Vec<Ident>,
    /// Phase 4F: Derive attributes for procedural macros
    pub derive_attrs: Vec<DeriveAttribute>,
    /// Phase 4F: Custom attributes
    pub custom_attrs: Vec<CustomAttribute>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Import {
    #[serde(with = "rc_str_serde")]
    pub path: Rc<str>,
    pub items: Vec<ImportItem>,
    pub span: Span,
    pub file_type: FileType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ImportItem {
    pub name: Ident,
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Export {
    pub name: Ident,
    pub item: ExportItem,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ExportItem {
    Function(Ident),
    Struct(Ident),
    Const(Ident),
    All,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FileType {
    RustScript,
    JavaScript,
    TypeScript,
    JSON,
    WebAssembly,
    Unknown,
}

impl FileType {
    #[allow(dead_code)]
    pub fn from_path(path: &PathBuf) -> Self {
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("rscc") => FileType::RustScript,
            Some("js") | Some("mjs") | Some("cjs") => FileType::JavaScript,
            Some("ts") | Some("tsx") => FileType::TypeScript,
            Some("json") => FileType::JSON,
            Some("wasm") => FileType::WebAssembly,
            _ => FileType::Unknown,
        }
    }
    
    #[allow(dead_code)]
    pub fn is_rustscript(&self) -> bool {
        matches!(self, FileType::RustScript)
    }
    
    #[allow(dead_code)]
    pub fn expected_output_extension(&self, target: &str) -> &'static str {
        match (self, target) {
            (FileType::RustScript, "js") => "js",
            (FileType::RustScript, "wasm") => "wasm",
            (FileType::RustScript, "native") => "",
            (FileType::JavaScript, _) => "js",
            (FileType::TypeScript, _) => "js",
            _ => "",
        }
    }
}

/// Dialect rule for DSL (domain-specific language) definitions.
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DialectRule {
    pub pattern: Pattern,
    pub action: Expr,
    pub span: Span,
}

/// Trait definition for implementing behaviours on types.
/// 
/// Phase 4A: Traits enable zero-cost abstractions and RAII patterns.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Trait {
    pub name: Ident,
    pub methods: Vec<TraitMethod>,
    /// Phase 4A: Associated types
    pub associated_types: Vec<AssociatedType>,
    /// Phase 4A: Supertrait bounds (trait inheritance)
    pub supertraits: Vec<Ident>,
    /// Phase 4A: Generic type parameters with bounds
    pub type_params: Vec<TypeParam>,
    pub span: Span,
}

/// Phase 4A: Type parameter with optional trait bounds
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    /// Phase 4B: Enhanced type bounds
    pub bounds: Vec<TypeBound>,
    /// Phase 4B: Variance annotation
    pub variance: Option<Variance>,
}

/// Phase 4B: Variance annotations for type parameters
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Variance {
    /// Covariant (+T) - can substitute with subtypes
    Covariant,
    /// Contravariant (-T) - can substitute with supertypes
    Contravariant,
    /// Invariant (T) - no substitution allowed (default)
    Invariant,
}

/// Phase 4B: Associated type with optional default and bounds
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AssociatedType {
    pub name: Ident,
    pub bounds: Vec<TypeBound>,
    pub default: Option<Type>,
}

/// A method signature in a trait definition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TraitMethod {
    pub name: Ident,
    pub params: Vec<(Pattern, Option<Type>, Option<Expr>)>,
    pub return_type: Option<Type>,
    /// Phase 4A: Default implementation
    pub default_impl: Option<Block>,
    pub span: Span,
}

/// Trait implementation for a specific type.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TraitImpl {
    pub trait_name: Ident,
    pub for_type: Type,
    pub methods: Vec<Function>,
    /// Phase 4A: Associated type implementations
    pub associated_type_impls: Vec<(Ident, Type)>,
    /// Phase 4A: Where clause constraints
    pub where_clause: Vec<WhereClause>,
    pub span: Span,
}

/// Extended type system utilities.
impl Type {
    /// Creates a union type (TypeScript-style: A | B | C).
    #[allow(dead_code)]
    pub fn union(types: Vec<Type>) -> Self {
        let len = types.len();
        Type::Generic(Ident {
            name: format!("Union<{len}>").into(),
            span: Span::default(),
        })
    }
    
    /// Creates an intersection type (TypeScript-style: A & B & C).
    #[allow(dead_code)]
    pub fn intersection(types: Vec<Type>) -> Self {
        let len = types.len();
        Type::Generic(Ident {
            name: format!("Intersection<{len}>").into(),
            span: Span::default(),
        })
    }
    
    /// Creates a reference type (Phase 4A: Borrowing).
    #[allow(dead_code)]
    pub fn reference(inner: Type, mutable: bool) -> Self {
        Type::Reference {
            inner: Box::new(inner),
            mutable,
            lifetime: None,
        }
    }
    
    /// Creates a reference type with a lifetime (Phase 4A: Lifetimes).
    #[allow(dead_code)]
    pub fn reference_with_lifetime(inner: Type, mutable: bool, lifetime: Lifetime) -> Self {
        Type::Reference {
            inner: Box::new(inner),
            mutable,
            lifetime: Some(lifetime),
        }
    }
}

/// Phase 4D: Select arm for channel multiplexing
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SelectArm {
    pub pattern: SelectPattern,
    pub body: Block,
}

/// Phase 4D: Pattern for select arms
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SelectPattern {
    /// Receive from channel: value = rx.recv()
    Recv {
        pattern: Pattern,
        channel: Expr,
    },
    /// Send to channel: tx.send(value)
    Send {
        channel: Expr,
        value: Expr,
    },
    /// Timeout: timeout(duration)
    Timeout(Expr),
    /// Default case
    Default,
}

/// Phase 4D: Atomic operations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AtomicOp {
    Load,
    Store,
    Swap,
    CompareExchange,
    FetchAdd,
    FetchSub,
    FetchAnd,
    FetchOr,
    FetchXor,
}

/// Phase 4D: Memory ordering for atomic operations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum MemoryOrdering {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}
