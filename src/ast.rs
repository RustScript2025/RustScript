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

/// Custom serde module for Rc<str> serialization
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
}

/// Custom serde module for Option<Rc<str>> serialization
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
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Pattern, Option<Type>)>,
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
    pub span: Span,
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
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>,
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
    pub fn from_path(path: &PathBuf) -> Self {
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("rjsc") => FileType::RustScript,
            Some("js") | Some("mjs") | Some("cjs") => FileType::JavaScript,
            Some("ts") | Some("tsx") => FileType::TypeScript,
            Some("json") => FileType::JSON,
            Some("wasm") => FileType::WebAssembly,
            _ => FileType::Unknown,
        }
    }
    
    pub fn is_rustscript(&self) -> bool {
        matches!(self, FileType::RustScript)
    }
    
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DialectRule {
    pub pattern: Pattern,
    pub action: Expr,
    pub span: Span,
}

/// Extended type system utilities.
impl Type {
    /// Creates a union type (TypeScript-style: A | B | C).
    pub fn union(types: Vec<Type>) -> Self {
        let len = types.len();
        Type::Generic(Ident {
            name: format!("Union<{len}>").into(),
            span: Span::default(),
        })
    }
    
    /// Creates an intersection type (TypeScript-style: A & B & C).
    pub fn intersection(types: Vec<Type>) -> Self {
        let len = types.len();
        Type::Generic(Ident {
            name: format!("Intersection<{len}>").into(),
            span: Span::default(),
        })
    }
}
