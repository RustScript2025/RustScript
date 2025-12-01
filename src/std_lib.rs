//! Standard library definitions for RustScript.
//! 
//! Author: Michael Lauzon
//! 
//! This module defines the built-in types, functions, and objects that are
//! available in all RustScript programmes. This includes console methods,
//! mathematical functions, and other runtime utilities.
//! 
//! The standard library is automatically available without imports.

use crate::ast::{Type, Ident, Span};
use std::rc::Rc;
use std::collections::HashMap;

/// The RustScript standard library.
/// 
/// This structure contains type information for all built-in globals,
/// which is used by the type checker to validate code that uses standard
/// library functions.
#[allow(dead_code)]
pub struct StdLib {
    /// Global variables and their types.
    pub globals: HashMap<Rc<str>, Type>,
}

impl StdLib {
    /// Creates a new standard library instance with all built-in definitions.
    /// 
    /// This initialises the global namespace with:
    /// - `console` object with `log` and `error` methods
    /// - `Math` object with mathematical constants and functions
    /// 
    /// Additional standard library features can be added here as the language evolves.
    #[allow(dead_code)]
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        
        // Console object for output
        let console_type = Type::Record(vec![
            (
                Ident { name: "log".into(), span: Span::default() },
                Type::Function {
                    params: vec![Type::String],
                    return_type: Box::new(Type::Tuple(vec![])),
                }
            ),
            (
                Ident { name: "error".into(), span: Span::default() },
                Type::Function {
                    params: vec![Type::String],
                    return_type: Box::new(Type::Tuple(vec![])),
                }
            ),
        ]);
        globals.insert("console".into(), console_type);
        
        // Math object for mathematical operations
        let math_type = Type::Record(vec![
            (
                Ident { name: "PI".into(), span: Span::default() },
                Type::Number
            ),
            (
                Ident { name: "sqrt".into(), span: Span::default() },
                Type::Function {
                    params: vec![Type::Number],
                    return_type: Box::new(Type::Number),
                }
            ),
        ]);
        globals.insert("Math".into(), math_type);
        
        Self { globals }
    }
}
