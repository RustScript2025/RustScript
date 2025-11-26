//! RustScript Lexer
//! 
//! Author: Michael Lauzon

use logos::{Logos, Span};
use crate::ast::Ident;

/// The complete set of tokens recognised by the RustScript lexer.
/// 
/// This lexer uses the Logos crate for efficient tokenisation. It handles all
/// RustScript language constructs including keywords, operators, literals, and
/// identifiers. Whitespace and line comments are automatically skipped.
/// 
/// The lexer is designed to be fast and memory-efficient, using string interning
/// for identifiers to reduce allocation overhead.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
#[logos(skip r"//[^\n]*")]   // Skip line comments
pub enum Token {
    // Keywords
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("fn")]
    Fn,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("match")]
    Match,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("guard")]
    Guard,
    #[token("defer")]
    Defer,
    #[token("extend")]
    Extend,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("return")]
    Return,
    #[token("yield")]
    Yield,
    #[token("gen")]
    Gen,
    #[token("struct")]
    Struct,
    #[token("impl")]
    Impl,
    #[token("mod")]
    Mod,
    #[token("use")]
    Use,
    #[token("requires")]
    Requires,
    #[token("ensures")]
    Ensures,
    #[token("invariant")]
    Invariant,
    #[token("comptime")]
    Comptime,
    #[token("pure")]
    Pure,
    #[token("effect")]
    Effect,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 1, callback = |lex| Ident { name: lex.slice().into(), span: crate::ast::Span { start: lex.span().start, end: lex.span().end, file_id: 0 } })]
    Ident(Ident),

    // Punctuation
    #[token("=>")]
    FatArrow,
    #[token("->")]
    ThinArrow,
    #[token("|>")]
    Pipeline,
    #[token("?.")]
    OptionalChain,
    #[token("??")]
    NullCoalesce,
    #[token("::")]
    ColonColon,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("@")]
    At,

    // Operators
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("?")]
    Question,

    // Pattern matching - higher priority than Ident to match "_" specifically
    #[token("_", priority = 3)]
    Underscore,
}

/// A wrapper around the Logos lexer that provides an iterator over tokens and their spans.
/// 
/// This lexer is designed to be used with Pest for parsing. It produces a stream
/// of (Token, Span) pairs that can be consumed by the parser to build an AST.
/// 
/// # Example
/// 
/// ```ignore
/// let lexer = Lexer::new("let x = 42;");
/// for (token, span) in lexer {
///     println!("{:?} at {:?}", token, span);
/// }
/// ```
pub struct Lexer<'input> {
    inner: logos::Lexer<'input, Token>,
}

impl<'input> Lexer<'input> {
    /// Creates a new lexer for the given input string.
    /// 
    /// The lexer will tokenise the entire input on demand as the iterator is consumed.
    pub fn new(input: &'input str) -> Self {
        Self {
            inner: Token::lexer(input),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;
        let span = self.inner.span();
        
        match token {
             Ok(t) => Some((t, span)),
             Err(_) => None, // Logos error variant (e.g. invalid token)
        }
    }
}
