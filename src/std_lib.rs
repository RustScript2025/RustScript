use crate::ast::{Type, Ident, Span};
use std::rc::Rc;
use std::collections::HashMap;

/// Defines the standard library for RustScript.
/// This includes built-in types, functions, and objects available in the global scope.
pub struct StdLib {
    pub globals: HashMap<Rc<str>, Type>,
}

impl StdLib {
    /// Create a new standard library instance.
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        
        // Console object
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
        
        // Math object
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
