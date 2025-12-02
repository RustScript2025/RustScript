//! Macro Registry and Built-in Macros

use crate::macro_system::*;
use crate::ast::*;
use std::collections::HashMap;

/// Registry of all available macros
pub struct MacroRegistry {
    declarative_macros: HashMap<String, MacroDefinition>,
    derive_macros: HashMap<String, DeriveMacro>,
}

impl MacroRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            declarative_macros: HashMap::new(),
            derive_macros: HashMap::new(),
        };
        
        registry.register_builtin_macros();
        registry
    }
    
    fn register_builtin_macros(&mut self) {
        self.register_vec_macro();
        self.register_assert_macros();
        self.register_derive_macros();
    }
    
    fn register_vec_macro(&mut self) {
        let vec_macro = MacroDefinition {
            name: Ident {
                name: "vec".to_string().into(),
                span: Span::default(),
            },
            rules: vec![
                MacroRule {
                    pattern: vec![],
                    expansion: vec![
                        MacroToken::Literal("Vec::new()".to_string()),
                    ],
                },
            ],
            span: Span::default(),
        };
        
        self.declarative_macros.insert("vec".to_string(), vec_macro);
    }
    
    fn register_assert_macros(&mut self) {
        let assert_macro = MacroDefinition {
            name: Ident {
                name: "assert".to_string().into(),
                span: Span::default(),
            },
            rules: vec![
                MacroRule {
                    pattern: vec![
                        MacroToken::Variable(Ident {
                            name: "condition".to_string().into(),
                            span: Span::default(),
                        }),
                    ],
                    expansion: vec![
                        MacroToken::Literal("if !(".to_string()),
                        MacroToken::Variable(Ident {
                            name: "condition".to_string().into(),
                            span: Span::default(),
                        }),
                        MacroToken::Literal(") { panic!(\"Assertion failed\"); }".to_string()),
                    ],
                },
            ],
            span: Span::default(),
        };
        
        self.declarative_macros.insert("assert".to_string(), assert_macro);
    }
    
    fn register_derive_macros(&mut self) {
        let derive_traits = [
            "Debug", "Clone", "PartialEq", "Eq", "Hash", "Copy",
            "Serialize", "Deserialize", "Default",
        ];
        
        for trait_name in &derive_traits {
            let derive_macro = DeriveMacro {
                trait_name: trait_name.to_string(),
            };
            
            self.derive_macros.insert(trait_name.to_string(), derive_macro);
        }
    }
    
    pub fn get_declarative_macro(&self, name: &str) -> Option<&MacroDefinition> {
        self.declarative_macros.get(name)
    }
    
    pub fn get_derive_macro(&self, name: &str) -> Option<&DeriveMacro> {
        self.derive_macros.get(name)
    }
    
    pub fn register_declarative_macro(&mut self, name: String, macro_def: MacroDefinition) {
        self.declarative_macros.insert(name, macro_def);
    }
    
    pub fn list_macros(&self) -> Vec<String> {
        let mut macros = Vec::new();
        macros.extend(self.declarative_macros.keys().cloned());
        macros.extend(self.derive_macros.keys().cloned());
        macros.sort();
        macros
    }
}

impl Default for MacroRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Macro expansion phase integration
pub struct MacroExpansionPhase {
    registry: MacroRegistry,
    expander: MacroExpander,
    code_generator: CodeGenerator,
}

impl MacroExpansionPhase {
    pub fn new() -> Self {
        let registry = MacroRegistry::new();
        let mut expander = MacroExpander::new();
        
        for (name, macro_def) in &registry.declarative_macros {
            expander.register_macro(name.clone(), macro_def.clone());
        }
        
        Self {
            registry,
            expander,
            code_generator: CodeGenerator::new(),
        }
    }
    
    pub fn expand_macros(&mut self, ast: &mut Vec<Stmt>) -> Result<(), MacroError> {
        let mut expanded_stmts = Vec::new();
        
        for stmt in ast.iter() {
            match self.expand_stmt(stmt) {
                Ok(mut new_stmts) => expanded_stmts.append(&mut new_stmts),
                Err(e) => return Err(e),
            }
        }
        
        *ast = expanded_stmts;
        Ok(())
    }
    
    fn expand_stmt(&mut self, stmt: &Stmt) -> Result<Vec<Stmt>, MacroError> {
        match stmt {
            Stmt::Expr(expr, span) => {
                let expanded_expr = self.expand_expr(expr)?;
                Ok(vec![Stmt::Expr(expanded_expr, span.clone())])
            }
            _ => Ok(vec![stmt.clone()]),
        }
    }
    
    fn expand_expr(&mut self, expr: &Expr) -> Result<Expr, MacroError> {
        match expr {
            Expr::MacroInvocation { name, args, span } => {
                let invocation = MacroInvocation {
                    name: name.clone(),
                    args: args.clone(),
                    span: span.clone(),
                };
                
                let token_stream = self.expander.expand_macro(&invocation)?;
                let _expanded_stmts = self.expander.tokens_to_ast(&token_stream)?;
                
                Ok(Expr::Literal(Literal::Number(0.0), span.clone()))
            }
            Expr::Quote { code: _, span } => {
                let generated_stmts = self.code_generator.generate_from_quote(expr)?;
                self.code_generator.emit_code(generated_stmts)?;
                
                Ok(Expr::Literal(Literal::Number(0.0), span.clone()))
            }
            _ => Ok(expr.clone()),
        }
    }
    
    pub fn process_derive_attributes(&mut self, structs: &mut [Struct]) -> Result<Vec<Stmt>, MacroError> {
        let mut generated_impls = Vec::new();
        
        for struct_def in structs {
            for derive_attr in &struct_def.derive_attrs {
                for trait_name in &derive_attr.traits {
                    if let Some(derive_macro) = self.registry.get_derive_macro(&trait_name.name) {
                        let input_tokens = self.struct_to_tokens(struct_def)?;
                        let impl_tokens = derive_macro.expand(input_tokens)?;
                        let impl_stmts = self.expander.tokens_to_ast(&impl_tokens)?;
                        generated_impls.extend(impl_stmts);
                    }
                }
            }
        }
        
        Ok(generated_impls)
    }
    
    fn struct_to_tokens(&self, struct_def: &Struct) -> Result<TokenStream, MacroError> {
        let mut tokens = vec![
            Token::Keyword("struct".to_string(), Span::default()),
            Token::Ident(struct_def.name.name.to_string(), struct_def.name.span.clone()),
        ];
        
        let field_tokens: Vec<Token> = struct_def.fields.iter().flat_map(|(name, _type)| {
            vec![
                Token::Ident(name.name.to_string(), name.span.clone()),
                Token::Punct(':', Span::default()),
                Token::Ident("Type".to_string(), Span::default()),
                Token::Punct(',', Span::default()),
            ]
        }).collect();
        
        tokens.push(Token::Group(
            Delimiter::Brace,
            TokenStream {
                tokens: field_tokens,
                span: Span::default(),
            },
            Span::default(),
        ));
        
        Ok(TokenStream {
            tokens,
            span: Span::default(),
        })
    }
}

impl Default for MacroExpansionPhase {
    fn default() -> Self {
        Self::new()
    }
}
