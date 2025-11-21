use logos::{Logos, Span};
use crate::ast::Ident;
use std::rc::Rc;

/// The complete set of tokens for RustScript.
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
    #[token("struct")]
    Struct,
    #[token("impl")]
    Impl,
    #[token("mod")]
    Mod,
    #[token("use")]
    Use,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Ident { name: lex.slice().into(), span: crate::ast::Span { start: lex.span().start, end: lex.span().end, file_id: 0 } })]
    Ident(Ident),

    // Punctuation
    #[token("=>")]
    FatArrow,
    #[token("->")]
    ThinArrow,
    #[token("|>")]
    Pipeline,
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

    // Pattern matching
    #[token("_")]
    Underscore,
}

/// A wrapper around the `logos::Lexer` that provides an iterator over `(Token, Span)`.
pub struct Lexer<'input> {
    inner: logos::Lexer<'input, Token>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for the given input string.
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
