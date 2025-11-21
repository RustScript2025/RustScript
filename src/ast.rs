use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Ident {
    pub name: Rc<str>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Record(Vec<(Ident, Type)>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Generic(Ident),
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
    String(Rc<str>),
    Boolean(bool),
    Array(Vec<Expr>),
    Record(Vec<(Ident, Expr)>),
    Tuple(Vec<Expr>),
    Regex(Rc<str>),
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
        args: Vec<(Option<Ident>, Expr)>, // Updated for named args
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
    Do(Box<Expr>, Span),
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
    pub span: Span,
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
    Struct(Struct),
    Import(Import),
    Extend {
        target: Type,
        methods: Vec<Function>,
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
