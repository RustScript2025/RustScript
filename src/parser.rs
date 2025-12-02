

//! Parser for RustScript using pest.
//!
//! Author: Michael Lauzon
//!
//! This module uses the pest parsing library to parse RustScript source code
//! and convert it into an Abstract Syntax Tree (AST). The grammar is defined
//! in rustscript.pest and uses PEG (Parsing Expression Grammar) semantics.

use pest::Parser;
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use std::rc::Rc;
use crate::ast::*;

#[derive(pest_derive::Parser)]
#[grammar = "rustscript.pest"]
pub struct RustScriptParser;

lazy_static::lazy_static! {
    static ref PRATT: PrattParser<Rule> = {
        use Assoc::*;
        PrattParser::new()
            .op(Op::infix(Rule::expr_pipeline, Left))
            .op(Op::infix(Rule::expr_null_coalesce, Left))
            .op(Op::infix(Rule::expr_logical_or, Left))
            .op(Op::infix(Rule::expr_logical_and, Left))
            .op(Op::infix(Rule::comparison_op, Left))
            .op(Op::infix(Rule::expr_additive, Left))
            .op(Op::infix(Rule::expr_multiplicative, Left))
    };
}

/// Parse a complete RustScript programme from source code.
///
/// # Arguments
///
/// * `source` - The source code to parse
///
/// # Returns
///
/// A Module AST node representing the parsed programme, or an error if parsing fails.
pub fn parse_program(source: &str) -> Result<Module, pest::error::Error<Rule>> {
    let mut pairs = RustScriptParser::parse(Rule::program, source)?;
    
    let mut items = Vec::new();
    
    // Get the programme pair first, then iterate its inner pairs
    if let Some(program_pair) = pairs.next() {
        for pair in program_pair.into_inner() {
            match pair.as_rule() {
                Rule::item => {
                    items.push(parse_item(pair)?);
                }
                Rule::EOI => {}
                _ => {}
            }
        }
    }
    
    Ok(Module {
        name: Ident {
            name: "main".into(),
            span: Span::default(),
        },
        items,
        span: Span::default(),
        file_path: std::path::PathBuf::from("unknown"),
        exports: Vec::new(),
    })
}

fn parse_item(pair: Pair<Rule>) -> Result<Item, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::function => Ok(Item::Function(parse_function(inner)?)),
        Rule::struct_def => Ok(Item::Struct(parse_struct(inner)?)),
        Rule::enum_def => Ok(Item::Enum(parse_enum(inner)?)),
        Rule::extend_block => parse_extend_block(inner),
        Rule::import => Ok(Item::Import(parse_import(inner)?)),
        Rule::trait_def => Ok(Item::Trait(parse_trait_def(inner)?)),
        Rule::trait_impl => Ok(Item::TraitImpl(parse_trait_impl(inner)?)),
        Rule::type_alias => Ok(Item::TypeAlias(parse_type_alias(inner)?)),
        Rule::type_fn_def => Ok(Item::TypeFunction(parse_type_function(inner)?)),
        _ => unreachable!(),
    }
}

fn parse_trait_def(pair: Pair<Rule>) -> Result<Trait, pest::error::Error<Rule>> {
    let mut name = None;
    let mut methods = Vec::new();
    let mut associated_types = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::trait_item => {
                // Phase 4B: Parse trait items (methods or associated types)
                let item_inner = inner.into_inner().next().unwrap();
                match item_inner.as_rule() {
                    Rule::trait_method => {
                        methods.push(parse_trait_method(item_inner)?);
                    }
                    Rule::trait_associated_type => {
                        associated_types.push(parse_associated_type(item_inner)?);
                    }
                    _ => {}
                }
            }
            Rule::trait_method => {
                // Backwards compatibility
                methods.push(parse_trait_method(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Trait {
        name: name.unwrap(),
        methods,
        associated_types,
        supertraits: Vec::new(), // Phase 4A: Supertraits
        type_params: Vec::new(), // Phase 4A: Type parameters
        span: Span::default(),
    })
}

fn parse_trait_method(pair: Pair<Rule>) -> Result<TraitMethod, pest::error::Error<Rule>> {
    let mut name = None;
    let mut params = Vec::new();
    let mut return_type = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::params => {
                params = parse_params(inner)?;
            }
            Rule::type_expr => {
                return_type = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(TraitMethod {
        name: name.unwrap(),
        params,
        return_type,
        default_impl: None, // Phase 4A: Default implementation
        span: Span::default(),
    })
}

fn parse_trait_impl(pair: Pair<Rule>) -> Result<TraitImpl, pest::error::Error<Rule>> {
    let mut trait_name = None;
    let mut for_type = None;
    let mut methods = Vec::new();
    let mut associated_type_impls = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if trait_name.is_none() {
                    trait_name = Some(parse_ident(inner));
                }
            }
            Rule::type_expr => {
                for_type = Some(parse_type(inner)?);
            }
            Rule::impl_item => {
                // Phase 4G: Parse impl items (functions or associated types)
                for item in inner.into_inner() {
                    match item.as_rule() {
                        Rule::function => {
                            methods.push(parse_function(item)?);
                        }
                        Rule::impl_associated_type => {
                            let (name, ty) = parse_impl_associated_type(item)?;
                            associated_type_impls.push((name, ty));
                        }
                        _ => {}
                    }
                }
            }
            Rule::function => {
                // Legacy support for direct functions
                methods.push(parse_function(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(TraitImpl {
        trait_name: trait_name.unwrap(),
        for_type: for_type.unwrap(),
        methods,
        associated_type_impls,
        where_clause: Vec::new(), // Phase 4A: Where clause
        span: Span::default(),
    })
}

// Phase 4G: Parse associated type in impl block
fn parse_impl_associated_type(pair: Pair<Rule>) -> Result<(Ident, Type), pest::error::Error<Rule>> {
    let mut name = None;
    let mut ty = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                name = Some(parse_ident(inner));
            }
            Rule::type_expr => {
                ty = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok((name.unwrap(), ty.unwrap()))
}

fn parse_function(pair: Pair<Rule>) -> Result<Function, pest::error::Error<Rule>> {
    let mut async_ = false;
    let mut generator = false;
    let mut is_pure = false;
    let mut memoized = false;
    let mut const_fn = false;
    let mut name = None;
    let mut params = Vec::new();
    let mut return_type = None;
    let mut contracts = Vec::new();
    let mut body = None;
    let mut lifetimes = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::decorator => {
                // Phase 4C: Parse decorators
                let decorator_name = inner.into_inner().next().unwrap().as_str();
                if decorator_name == "memoize" {
                    memoized = true;
                }
            }
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::lifetime_params => {
                lifetimes = parse_lifetime_params(inner)?;
            }
            Rule::params => {
                params = parse_params(inner)?;
            }
            Rule::curried_params => {
                // Phase 4C: Parse curried parameters
                // For now, flatten all param groups into a single list
                for param_group in inner.into_inner() {
                    if param_group.as_rule() == Rule::param_group {
                        for params_rule in param_group.into_inner() {
                            if params_rule.as_rule() == Rule::params {
                                let group_params = parse_params(params_rule)?;
                                params.extend(group_params);
                            }
                        }
                    }
                }
            }
            Rule::type_expr => {
                return_type = Some(parse_type(inner)?);
            }
            Rule::contract => {
                contracts.push(parse_contract(inner)?);
            }
            Rule::block => {
                body = Some(parse_block(inner)?);
            }
            _ => {
                // Handle modifiers (async, gen, pure, const)
                match inner.as_str() {
                    "async" => async_ = true,
                    "gen" => generator = true,
                    "pure" => is_pure = true,
                    "const" => const_fn = true,
                    _ => {}
                }
            }
        }
    }
    
    Ok(Function {
        name: name.unwrap(),
        params,
        return_type,
        body: body.unwrap(),
        async_,
        generator,
        guard: None,
        contracts,
        effects: if is_pure { vec![Effect::Pure] } else { Vec::new() },
        lifetimes,
        const_params: Vec::new(), // Phase 4A: Const generics (parsed separately)
        tail_call_optimized: false, // Phase 4A: Will be set by optimiser
        where_clause: Vec::new(), // Phase 4B: Where clause (to be parsed)
        param_groups: Vec::new(), // Phase 4C: Curried parameter groups (to be parsed)
        memoized, // Phase 4C: Memoisation flag
        const_fn, // Phase 4H: Const function flag
        span: Span::default(),
    })
}

fn parse_lifetime_params(pair: Pair<Rule>) -> Result<Vec<Lifetime>, pest::error::Error<Rule>> {
    let mut lifetimes = Vec::new();
    
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::lifetime {
            lifetimes.push(parse_lifetime(inner)?);
        }
    }
    
    Ok(lifetimes)
}

fn parse_lifetime(pair: Pair<Rule>) -> Result<Lifetime, pest::error::Error<Rule>> {
    let ident = pair.into_inner().next().unwrap();
    Ok(Lifetime {
        name: ident.as_str().into(),
        span: Span::default(),
    })
}

fn parse_params(pair: Pair<Rule>) -> Result<Vec<(Pattern, Option<Type>, Option<Expr>)>, pest::error::Error<Rule>> {
    let mut params = Vec::new();
    
    for param_pair in pair.into_inner() {
        if param_pair.as_rule() == Rule::param {
            let mut pattern = None;
            let mut type_ann = None;
            let mut default_value = None;
            
            for inner in param_pair.into_inner() {
                match inner.as_rule() {
                    Rule::pattern => {
                        pattern = Some(parse_pattern(inner)?);
                    }
                    Rule::type_expr => {
                        type_ann = Some(parse_type(inner)?);
                    }
                    Rule::expr => {
                        default_value = Some(parse_expr(inner)?);
                    }
                    _ => {}
                }
            }
            
            params.push((pattern.unwrap(), type_ann, default_value));
        }
    }
    
    Ok(params)
}

fn parse_contract(pair: Pair<Rule>) -> Result<Contract, pest::error::Error<Rule>> {
    let mut kind = None;
    let mut condition = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::contract_kind => {
                kind = Some(match inner.as_str() {
                    "requires" => ContractKind::Requires,
                    "ensures" => ContractKind::Ensures,
                    "invariant" => ContractKind::Invariant,
                    _ => unreachable!(),
                });
            }
            Rule::expr => {
                condition = Some(parse_expr(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Contract {
        kind: kind.unwrap(),
        condition: condition.unwrap(),
        message: None,
        span: Span::default(),
    })
}

fn parse_struct(pair: Pair<Rule>) -> Result<Struct, pest::error::Error<Rule>> {
    let mut derive_attrs = Vec::new();
    let mut custom_attrs = Vec::new();
    let mut struct_def = None;
    
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::attribute => {
                let (derive, custom) = parse_attribute(item)?;
                if let Some(d) = derive {
                    derive_attrs.push(d);
                }
                if let Some(c) = custom {
                    custom_attrs.push(c);
                }
            }
            Rule::struct_tuple | Rule::struct_named => {
                struct_def = Some(item);
            }
            _ => {}
        }
    }
    
    let inner = struct_def.unwrap();
    
    match inner.as_rule() {
        Rule::struct_tuple => {
            // Phase 4B: Parse tuple struct (newtype pattern)
            let mut name = None;
            let mut tuple_fields = Vec::new();
            
            for tuple_inner in inner.into_inner() {
                match tuple_inner.as_rule() {
                    Rule::ident => {
                        if name.is_none() {
                            name = Some(parse_ident(tuple_inner));
                        }
                    }
                    Rule::type_expr => {
                        tuple_fields.push(parse_type(tuple_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Struct {
                name: name.unwrap(),
                fields: Vec::new(),
                const_params: Vec::new(),
                tuple_fields,
                phantom_params: Vec::new(),
                derive_attrs: derive_attrs.clone(),
                custom_attrs: custom_attrs.clone(),
                span: Span::default(),
            })
        }
        Rule::struct_named => {
            // Parse named struct
            let mut name = None;
            let mut fields = Vec::new();
            
            for named_inner in inner.into_inner() {
                match named_inner.as_rule() {
                    Rule::ident => {
                        if name.is_none() {
                            name = Some(parse_ident(named_inner));
                        }
                    }
                    Rule::struct_field_def => {
                        let mut field_name = None;
                        let mut field_type = None;
                        
                        for field_inner in named_inner.into_inner() {
                            match field_inner.as_rule() {
                                Rule::ident => {
                                    field_name = Some(parse_ident(field_inner));
                                }
                                Rule::type_expr => {
                                    field_type = Some(parse_type(field_inner)?);
                                }
                                _ => {}
                            }
                        }
                        
                        fields.push((field_name.unwrap(), field_type.unwrap()));
                    }
                    _ => {}
                }
            }
            
            Ok(Struct {
                name: name.unwrap(),
                fields,
                const_params: Vec::new(),
                tuple_fields: Vec::new(),
                phantom_params: Vec::new(),
                derive_attrs,
                custom_attrs,
                span: Span::default(),
            })
        }
        _ => unreachable!(),
    }
}

fn parse_type_params(pair: Pair<Rule>) -> Vec<Ident> {
    let mut params = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::type_param {
            // Parse type parameter
            for param_inner in inner.into_inner() {
                if param_inner.as_rule() == Rule::ident {
                    params.push(parse_ident(param_inner));
                    break; // Only take the first ident (the parameter name)
                }
            }
        }
    }
    params
}

fn parse_enum(pair: Pair<Rule>) -> Result<Enum, pest::error::Error<Rule>> {
    let mut name = None;
    let mut type_params = Vec::new();
    let mut variants = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::type_params => {
                type_params = parse_type_params(inner);
            }
            Rule::enum_variant => {
                variants.push(parse_enum_variant(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Enum {
        name: name.unwrap(),
        type_params,
        variants,
        span: Span::default(),
    })
}

fn parse_enum_variant(pair: Pair<Rule>) -> Result<EnumVariant, pest::error::Error<Rule>> {
    let mut name = None;
    let mut fields = VariantFields::Unit;
    let mut return_type = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                name = Some(parse_ident(inner));
            }
            Rule::enum_variant_fields => {
                fields = parse_variant_fields(inner)?;
            }
            Rule::type_expr => {
                return_type = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(EnumVariant {
        name: name.unwrap(),
        fields,
        return_type,
        span: Span::default(),
    })
}

fn parse_variant_fields(pair: Pair<Rule>) -> Result<VariantFields, pest::error::Error<Rule>> {
    let inner_pairs: Vec<_> = pair.into_inner().collect();
    
    if inner_pairs.is_empty() {
        return Ok(VariantFields::Unit);
    }
    
    match inner_pairs[0].as_rule() {
        Rule::enum_variant_field => {
            // Tuple variant
            let mut fields = Vec::new();
            for field_pair in inner_pairs {
                if field_pair.as_rule() == Rule::enum_variant_field {
                    fields.push(parse_type(field_pair)?);
                }
            }
            Ok(VariantFields::Tuple(fields))
        }
        Rule::struct_field_def => {
            // Named variant
            let mut fields = Vec::new();
            for field_pair in inner_pairs {
                if field_pair.as_rule() == Rule::struct_field_def {
                    let mut field_name = None;
                    let mut field_type = None;
                    
                    for field_inner in field_pair.into_inner() {
                        match field_inner.as_rule() {
                            Rule::ident => {
                                field_name = Some(parse_ident(field_inner));
                            }
                            Rule::type_expr => {
                                field_type = Some(parse_type(field_inner)?);
                            }
                            _ => {}
                        }
                    }
                    
                    fields.push((field_name.unwrap(), field_type.unwrap()));
                }
            }
            Ok(VariantFields::Named(fields))
        }
        _ => Ok(VariantFields::Unit),
    }
}

fn parse_extend_block(pair: Pair<Rule>) -> Result<Item, pest::error::Error<Rule>> {
    let mut target = None;
    let mut methods = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::type_expr => {
                target = Some(parse_type(inner)?);
            }
            Rule::function => {
                methods.push(parse_function(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Item::Extend {
        target: target.unwrap(),
        methods,
        span: Span::default(),
    })
}

fn parse_import(pair: Pair<Rule>) -> Result<Import, pest::error::Error<Rule>> {
    let mut path_parts = Vec::new();
    
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::import_path {
            for ident_pair in inner.into_inner() {
                if ident_pair.as_rule() == Rule::ident {
                    path_parts.push(ident_pair.as_str());
                }
            }
        }
    }
    
    let path: Rc<str> = path_parts.join("::").into();
    
    Ok(Import {
        path,
        items: Vec::new(),
        span: Span::default(),
        file_type: FileType::Unknown,
    })
}

fn parse_block(pair: Pair<Rule>) -> Result<Block, pest::error::Error<Rule>> {
    let mut stmts = Vec::new();
    let mut expr = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::stmt => {
                stmts.push(parse_stmt(inner)?);
            }
            Rule::expr => {
                expr = Some(Box::new(parse_expr(inner)?));
            }
            _ => {}
        }
    }
    
    Ok(Block {
        stmts,
        expr,
        span: Span::default(),
    })
}

fn parse_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::stmt_let => {
            let mut mutable = false;
            let mut pattern = None;
            let mut type_ann = None;
            let mut value = None;
            
            for stmt_inner in inner.into_inner() {
                match stmt_inner.as_rule() {
                    Rule::pattern => {
                        pattern = Some(parse_pattern(stmt_inner)?);
                    }
                    Rule::type_expr => {
                        type_ann = Some(parse_type(stmt_inner)?);
                    }
                    Rule::expr => {
                        value = Some(parse_expr(stmt_inner)?);
                    }
                    _ => {
                        if stmt_inner.as_str() == "mut" {
                            mutable = true;
                        }
                    }
                }
            }
            
            Ok(Stmt::Let {
                mutable,
                pattern: pattern.unwrap(),
                type_ann,
                value,
                span: Span::default(),
            })
        }
        Rule::stmt_return => {
            let mut value = None;
            
            for ret_inner in inner.into_inner() {
                if ret_inner.as_rule() == Rule::expr {
                    value = Some(parse_expr(ret_inner)?);
                }
            }
            
            Ok(Stmt::Return(value, Span::default()))
        }
        Rule::stmt_guard => {
            let mut condition = None;
            let mut else_block = None;
            
            for guard_inner in inner.into_inner() {
                match guard_inner.as_rule() {
                    Rule::expr => {
                        condition = Some(parse_expr(guard_inner)?);
                    }
                    Rule::block => {
                        else_block = Some(parse_block(guard_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Stmt::Guard {
                condition: condition.unwrap(),
                else_block: else_block.unwrap(),
                span: Span::default(),
            })
        }
        Rule::stmt_defer => {
            let mut block = None;
            
            for defer_inner in inner.into_inner() {
                if defer_inner.as_rule() == Rule::block {
                    block = Some(parse_block(defer_inner)?);
                }
            }
            
            Ok(Stmt::Defer {
                block: block.unwrap(),
                span: Span::default(),
            })
        }
        Rule::stmt_break => {
            // Phase 4E: Parse break statement
            let mut label = None;
            let mut value = None;
            
            for break_inner in inner.into_inner() {
                match break_inner.as_rule() {
                    Rule::label => {
                        label = Some(parse_ident(break_inner.into_inner().next().unwrap()));
                    }
                    Rule::expr => {
                        value = Some(parse_expr(break_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Stmt::Break {
                label,
                value,
                span: Span::default(),
            })
        }
        Rule::stmt_continue => {
            // Phase 4E: Parse continue statement
            let mut label = None;
            
            for cont_inner in inner.into_inner() {
                if cont_inner.as_rule() == Rule::label {
                    label = Some(parse_ident(cont_inner.into_inner().next().unwrap()));
                }
            }
            
            Ok(Stmt::Continue {
                label,
                span: Span::default(),
            })
        }
        Rule::stmt_expr => {
            let expr_pair = inner.into_inner().next().unwrap();
            Ok(Stmt::Expr(parse_expr(expr_pair)?, Span::default()))
        }
        _ => unreachable!(),
    }
}

fn parse_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    parse_expr_hierarchy(inner)
}

fn parse_expr_hierarchy(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    match pair.as_rule() {
        Rule::expr_catch => parse_catch_expr(pair),
        Rule::expr_composition => parse_composition_chain(pair),
        Rule::expr_applicative => parse_applicative_chain(pair),
        Rule::expr_pipeline => parse_binary_chain(pair, BinaryOp::Pipeline),
        Rule::expr_null_coalesce => parse_null_coalesce_chain(pair),
        Rule::expr_logical_or => parse_binary_chain(pair, BinaryOp::Or),
        Rule::expr_logical_and => parse_binary_chain(pair, BinaryOp::And),
        Rule::expr_comparison => parse_comparison_chain(pair),
        Rule::expr_additive => parse_additive_chain(pair),
        Rule::expr_multiplicative => parse_multiplicative_chain(pair),
        Rule::expr_range => parse_range_expr(pair),
        Rule::expr_postfix => parse_postfix_expr(pair),
        Rule::expr_primary => parse_expr_primary(pair),
        _ => parse_expr_primary(pair),
    }
}

// Phase 4C: Parse applicative functor chain (<$> or <*>)
fn parse_applicative_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut parts: Vec<_> = pair.into_inner().collect();
    
    if parts.len() == 1 {
        return parse_expr_hierarchy(parts.pop().unwrap());
    }
    
    let mut exprs = Vec::new();
    let mut ops = Vec::new();
    
    let mut i = 0;
    while i < parts.len() {
        if i % 2 == 0 {
            exprs.push(parse_expr_hierarchy(parts[i].clone())?);
        } else {
            let op_str = parts[i].as_str();
            ops.push(if op_str == "<$>" {
                BinaryOp::ApplicativeFmap
            } else {
                BinaryOp::ApplicativeApply
            });
        }
        i += 1;
    }
    
    let mut result = exprs[0].clone();
    for (i, op) in ops.iter().enumerate() {
        result = Expr::Binary {
            left: Box::new(result),
            op: op.clone(),
            right: Box::new(exprs[i + 1].clone()),
            span: Span::default(),
        };
    }
    
    Ok(result)
}

// Phase 4C: Parse function composition chain (>> or <<)
fn parse_composition_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut parts: Vec<_> = pair.into_inner().collect();
    
    if parts.len() == 1 {
        return parse_expr_hierarchy(parts.pop().unwrap());
    }
    
    // Parse the chain and determine operators
    let mut exprs = Vec::new();
    let mut ops = Vec::new();
    
    let mut i = 0;
    while i < parts.len() {
        if i % 2 == 0 {
            // Expression
            exprs.push(parse_expr_hierarchy(parts[i].clone())?);
        } else {
            // Operator (>> or <<)
            let op_str = parts[i].as_str();
            ops.push(if op_str == ">>" {
                BinaryOp::ComposeForward
            } else {
                BinaryOp::ComposeBackward
            });
        }
        i += 1;
    }
    
    // Build the composition chain
    let mut result = exprs[0].clone();
    for (i, op) in ops.iter().enumerate() {
        result = Expr::Binary {
            left: Box::new(result),
            op: op.clone(),
            right: Box::new(exprs[i + 1].clone()),
            span: Span::default(),
        };
    }
    
    Ok(result)
}

fn parse_binary_chain(pair: Pair<Rule>, op: BinaryOp) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    let mut left = parse_expr_hierarchy(first)?;
    
    for next in inner {
        let right = parse_expr_hierarchy(next)?;
        let op_copy = match op {
            BinaryOp::Add => BinaryOp::Add,
            BinaryOp::Sub => BinaryOp::Sub,
            BinaryOp::Mul => BinaryOp::Mul,
            BinaryOp::Div => BinaryOp::Div,
            BinaryOp::Eq => BinaryOp::Eq,
            BinaryOp::Neq => BinaryOp::Neq,
            BinaryOp::Lt => BinaryOp::Lt,
            BinaryOp::Gt => BinaryOp::Gt,
            BinaryOp::Leq => BinaryOp::Leq,
            BinaryOp::Geq => BinaryOp::Geq,
            BinaryOp::And => BinaryOp::And,
            BinaryOp::Or => BinaryOp::Or,
            BinaryOp::Pipeline => BinaryOp::Pipeline,
            BinaryOp::ComposeForward => BinaryOp::ComposeForward,
            BinaryOp::ComposeBackward => BinaryOp::ComposeBackward,
            BinaryOp::ApplicativeFmap => BinaryOp::ApplicativeFmap,
            BinaryOp::ApplicativeApply => BinaryOp::ApplicativeApply,
        };
        left = Expr::Binary {
            left: Box::new(left),
            op: op_copy,
            right: Box::new(right),
            span: Span::default(),
        };
    }
    
    Ok(left)
}

fn parse_null_coalesce_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    let mut left = parse_expr_hierarchy(first)?;
    
    for next in inner {
        let right = parse_expr_hierarchy(next)?;
        left = Expr::NullCoalesce {
            left: Box::new(left),
            right: Box::new(right),
            span: Span::default(),
        };
    }
    
    Ok(left)
}

fn parse_comparison_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner = pair.into_inner().peekable();
    let first = inner.next().unwrap();
    let mut left = parse_expr_hierarchy(first)?;
    
    while let Some(next) = inner.next() {
        // Check if this is the comparison_op rule
        if next.as_rule() == Rule::comparison_op {
            let op = match next.as_str() {
                "==" => BinaryOp::Eq,
                "!=" => BinaryOp::Neq,
                "<" => BinaryOp::Lt,
                ">" => BinaryOp::Gt,
                "<=" => BinaryOp::Leq,
                ">=" => BinaryOp::Geq,
                _ => BinaryOp::Eq,
            };
            if let Some(right_pair) = inner.next() {
                let right = parse_expr_hierarchy(right_pair)?;
                left = Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span: Span::default(),
                };
            }
        } else {
            // Not an operator, must be an operand - recurse
            let right = parse_expr_hierarchy(next)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Eq,
                right: Box::new(right),
                span: Span::default(),
            };
        }
    }
    
    Ok(left)
}

fn parse_additive_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    let mut left = parse_expr_hierarchy(first)?;
    
    // In the grammar, expr_additive contains alternating operands
    // The operator is determined by checking if there are more operands
    // For simplicity, we assume + for now (the grammar structure handles precedence)
    for next in inner {
        let right = parse_expr_hierarchy(next)?;
        left = Expr::Binary {
            left: Box::new(left),
            op: BinaryOp::Add,
            right: Box::new(right),
            span: Span::default(),
        };
    }
    
    Ok(left)
}

fn parse_multiplicative_chain(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    let mut left = parse_expr_hierarchy(first)?;
    
    // Similar to additive - just chain the operands with *
    for next in inner {
        let right = parse_expr_hierarchy(next)?;
        left = Expr::Binary {
            left: Box::new(left),
            op: BinaryOp::Mul,
            right: Box::new(right),
            span: Span::default(),
        };
    }
    
    Ok(left)
}

// Phase 4G: Parse range expression (start..end or start..end::step)
fn parse_range_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    // Check if this is actually a range (contains ..)
    let text = pair.as_str();
    let parts: Vec<_> = pair.into_inner().collect();
    
    if parts.is_empty() {
        return Err(pest::error::Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: "Empty range expression".to_string(),
            },
            parts[0].as_span().start_pos(),
        ));
    }
    if !text.contains("..") {
        // Not a range, just a regular expression
        return parse_expr_hierarchy(parts[0].clone());
    }
    
    let mut start = None;
    let mut end = None;
    let mut step = None;
    
    // Parse the parts
    let mut i = 0;
    while i < parts.len() {
        let part = &parts[i];
        let part_text = part.as_str();
        
        if part_text == ".." {
            // Range operator found
            if i > 0 {
                start = Some(Box::new(parse_expr_hierarchy(parts[i - 1].clone())?));
            }
            if i + 1 < parts.len() && parts[i + 1].as_str() != "::" {
                end = Some(Box::new(parse_expr_hierarchy(parts[i + 1].clone())?));
            }
        } else if part_text == "::" {
            // Step operator
            if i + 1 < parts.len() {
                step = Some(Box::new(parse_expr_hierarchy(parts[i + 1].clone())?));
            }
        }
        i += 1;
    }
    
    Ok(Expr::Range {
        start,
        end,
        step,
        span: Span::default(),
    })
}

fn parse_expr_primary(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    match pair.as_rule() {
        Rule::expr_primary => {
            let inner = pair.into_inner().next().unwrap();
            parse_expr_primary(inner)
        }
        Rule::literal => {
            parse_literal(pair)
        }
        Rule::ident => {
            Ok(Expr::Ident(parse_ident(pair)))
        }
        Rule::expr_if => {
            parse_if_expr(pair)
        }
        Rule::expr_match => {
            parse_match_expr(pair)
        }
        Rule::expr_loop => {
            let mut body = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    body = Some(parse_block(inner)?);
                }
            }
            Ok(Expr::Loop {
                body: body.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_while => {
            let mut condition = None;
            let mut body = None;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::expr => {
                        condition = Some(Box::new(parse_expr(inner)?));
                    }
                    Rule::block => {
                        body = Some(parse_block(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::While {
                condition: condition.unwrap(),
                body: body.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_block => {
            let block_pair = pair.into_inner().next().unwrap();
            Ok(Expr::Block(parse_block(block_pair)?))
        }
        Rule::expr_struct_init => {
            parse_struct_init(pair)
        }
        Rule::expr_list_comp => {
            parse_list_comp(pair)
        }
        Rule::expr_yield => {
            let mut value = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    value = Some(Box::new(parse_expr(inner)?));
                }
            }
            Ok(Expr::Yield {
                value,
                span: Span::default(),
            })
        }
        Rule::expr_async => {
            let mut block = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    block = Some(parse_block(inner)?);
                }
            }
            Ok(Expr::Async(
                Box::new(Expr::Block(block.unwrap())),
                Span::default(),
            ))
        }
        Rule::expr_comptime => {
            let mut block = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    block = Some(parse_block(inner)?);
                }
            }
            Ok(Expr::Comptime {
                block: block.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_move => {
            // Phase 4A: Parse move expression
            let inner = pair.into_inner().next().unwrap();
            Ok(Expr::Move {
                expr: Box::new(parse_expr_primary(inner)?),
                span: Span::default(),
            })
        }
        Rule::expr_borrow => {
            // Phase 4A: Parse borrow expression (&expr or &mut expr)
            let mut mutable = false;
            let mut expr = None;
            
            for inner in pair.into_inner() {
                match inner.as_str() {
                    "mut" => mutable = true,
                    _ => {
                        if inner.as_rule() == Rule::expr_primary {
                            expr = Some(Box::new(parse_expr_primary(inner)?));
                        }
                    }
                }
            }
            
            Ok(Expr::Borrow {
                expr: expr.unwrap(),
                mutable,
                span: Span::default(),
            })
        }
        Rule::expr_perform => {
            // Phase 4A: Parse perform expression
            let mut effect = None;
            let mut args = Vec::new();
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::ident => {
                        effect = Some(parse_ident(inner));
                    }
                    Rule::expr => {
                        args.push(parse_expr(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Perform {
                effect: effect.unwrap(),
                args,
                span: Span::default(),
            })
        }
        Rule::expr_handle => {
            // Phase 4A: Parse handle expression
            let mut body = None;
            let mut handlers = Vec::new();
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::block => {
                        body = Some(parse_block(inner)?);
                    }
                    Rule::effect_handler => {
                        handlers.push(parse_effect_handler(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Handle {
                body: body.unwrap(),
                handlers,
                span: Span::default(),
            })
        }
        Rule::expr_resume => {
            // Phase 4A: Parse resume expression
            let mut value = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    value = Some(Box::new(parse_expr(inner)?));
                }
            }
            
            Ok(Expr::Resume {
                value: value.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_asm => {
            // Phase 4A: Parse inline assembly
            let mut instructions = Vec::new();
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::wasm_instruction {
                    instructions.push(parse_wasm_instruction(inner)?);
                }
            }
            
            Ok(Expr::InlineAsm {
                instructions,
                span: Span::default(),
            })
        }
        Rule::expr_placeholder => {
            // Phase 4C: Parse placeholder for partial application
            Ok(Expr::Placeholder(Span::default()))
        }
        Rule::expr_do => {
            // Phase 4C: Parse monadic do-notation
            let mut bindings = Vec::new();
            let mut result = None;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::do_binding => {
                        let mut pattern = None;
                        let mut expr = None;
                        
                        for binding_inner in inner.into_inner() {
                            match binding_inner.as_rule() {
                                Rule::pattern => {
                                    pattern = Some(parse_pattern(binding_inner)?);
                                }
                                Rule::expr => {
                                    expr = Some(parse_expr(binding_inner)?);
                                }
                                _ => {}
                            }
                        }
                        
                        bindings.push(crate::ast::DoBinding {
                            pattern: pattern.unwrap(),
                            expr: expr.unwrap(),
                        });
                    }
                    Rule::expr => {
                        result = Some(parse_expr(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Do {
                bindings,
                result: Box::new(result.unwrap()),
                span: Span::default(),
            })
        }
        Rule::expr_postfix => {
            parse_postfix_expr(pair)
        }
        Rule::expr_spawn => {
            // Phase 4D: Parse spawn expression
            let mut body = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    body = Some(parse_block(inner)?);
                }
            }
            Ok(Expr::Spawn {
                body: body.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_channel => {
            // Phase 4D: Parse channel creation
            let mut buffer_size = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    buffer_size = Some(Box::new(parse_expr(inner)?));
                }
            }
            Ok(Expr::Channel {
                buffer_size,
                span: Span::default(),
            })
        }
        Rule::expr_select => {
            // Phase 4D: Parse select expression
            let mut arms = Vec::new();
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::select_arm {
                    arms.push(parse_select_arm(inner)?);
                }
            }
            Ok(Expr::Select {
                arms,
                span: Span::default(),
            })
        }
        Rule::expr_scope => {
            // Phase 4D: Parse scoped threads
            let mut body = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    body = Some(parse_block(inner)?);
                }
            }
            Ok(Expr::Scope {
                body: body.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_atomic => {
            // Phase 4D: Parse atomic operation
            let mut operation = None;
            let mut target = None;
            let mut value = None;
            let mut ordering = crate::ast::MemoryOrdering::SeqCst;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::atomic_op => {
                        operation = Some(parse_atomic_op(inner)?);
                    }
                    Rule::expr => {
                        if target.is_none() {
                            target = Some(Box::new(parse_expr(inner)?));
                        } else {
                            value = Some(Box::new(parse_expr(inner)?));
                        }
                    }
                    Rule::memory_ordering => {
                        ordering = parse_memory_ordering(inner)?;
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Atomic {
                operation: operation.unwrap(),
                target: target.unwrap(),
                value,
                ordering,
                span: Span::default(),
            })
        }
        Rule::expr_future_join => {
            // Phase 4D: Parse Future::join
            let mut futures = Vec::new();
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    futures.push(parse_expr(inner)?);
                }
            }
            Ok(Expr::FutureJoin {
                futures,
                span: Span::default(),
            })
        }
        Rule::expr_future_select => {
            // Phase 4D: Parse Future::select
            let mut futures = Vec::new();
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    futures.push(parse_expr(inner)?);
                }
            }
            Ok(Expr::FutureSelect {
                futures,
                span: Span::default(),
            })
        }
        Rule::expr_future_race => {
            // Phase 4D: Parse Future::race
            let mut futures = Vec::new();
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    futures.push(parse_expr(inner)?);
                }
            }
            Ok(Expr::FutureRace {
                futures,
                span: Span::default(),
            })
        }
        Rule::expr_timeout => {
            // Phase 4D: Parse timeout
            let mut duration = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    duration = Some(Box::new(parse_expr(inner)?));
                }
            }
            Ok(Expr::Timeout {
                duration: duration.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_stream_from_iter => {
            // Phase 4D: Parse Stream::from_iter
            let mut iter = None;
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    iter = Some(Box::new(parse_expr(inner)?));
                }
            }
            Ok(Expr::StreamFromIter {
                iter: iter.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_try => {
            // Phase 4E: Parse try-catch block
            let mut body = None;
            let mut catch_clauses = Vec::new();
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::block => {
                        body = Some(parse_block(inner)?);
                    }
                    Rule::catch_clause => {
                        catch_clauses.push(parse_catch_clause(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Try {
                body: body.unwrap(),
                catch_clauses,
                span: Span::default(),
            })
        }
        Rule::expr_guard => {
            // Phase 4E: Parse guard clause
            let mut condition = None;
            let mut else_block = None;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::expr => {
                        condition = Some(Box::new(parse_expr(inner)?));
                    }
                    Rule::block => {
                        else_block = Some(parse_block(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::Guard {
                condition: condition.unwrap(),
                else_block: else_block.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_labeled_block => {
            // Phase 4E: Parse labelled block
            let mut label = None;
            let mut block = None;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::label => {
                        label = Some(parse_ident(inner.into_inner().next().unwrap()));
                    }
                    Rule::block => {
                        block = Some(parse_block(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::LabeledBlock {
                label: label.unwrap(),
                block: block.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_panic => {
            // Phase 4E: Parse panic expression
            let mut message = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    message = Some(Box::new(parse_expr(inner)?));
                }
            }
            
            Ok(Expr::Panic {
                message,
                span: Span::default(),
            })
        }
        Rule::expr_cfg => {
            // Phase 4E: Parse conditional compilation
            let mut condition = None;
            let mut then_expr = None;
            let mut else_expr = None;
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::cfg_condition => {
                        condition = Some(parse_cfg_condition(inner)?);
                    }
                    Rule::expr => {
                        if then_expr.is_none() {
                            then_expr = Some(Box::new(parse_expr(inner)?));
                        } else {
                            else_expr = Some(Box::new(parse_expr(inner)?));
                        }
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::CfgExpr {
                condition: condition.unwrap(),
                then_expr: then_expr.unwrap(),
                else_expr,
                span: Span::default(),
            })
        }
        Rule::expr_const_assert => {
            // Phase 4E: Parse const assertion
            let mut condition = None;
            let mut message = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    if condition.is_none() {
                        condition = Some(Box::new(parse_expr(inner)?));
                    } else {
                        message = Some(Box::new(parse_expr(inner)?));
                    }
                }
            }
            
            Ok(Expr::ConstAssert {
                condition: condition.unwrap(),
                message,
                span: Span::default(),
            })
        }
        Rule::expr_unreachable => {
            // Phase 4E: Parse unreachable marker
            let mut message = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::expr {
                    message = Some(Box::new(parse_expr(inner)?));
                }
            }
            
            Ok(Expr::Unreachable {
                message,
                span: Span::default(),
            })
        }
        Rule::expr_macro_invocation => {
            // Phase 4F: Parse macro invocation
            let mut name = None;
            let mut args = Vec::new();
            
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::ident => {
                        name = Some(parse_ident(inner));
                    }
                    Rule::expr => {
                        args.push(parse_expr(inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Expr::MacroInvocation {
                name: name.unwrap(),
                args,
                span: Span::default(),
            })
        }
        Rule::expr_type_info => {
            // Phase 4F: Parse type reflection
            let mut type_expr = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::type_expr {
                    type_expr = Some(Box::new(parse_type(inner)?));
                }
            }
            
            Ok(Expr::TypeInfo {
                type_expr: type_expr.unwrap(),
                span: Span::default(),
            })
        }
        Rule::expr_quote => {
            // Phase 4F: Parse quote expression
            let mut code = None;
            
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::block {
                    code = Some(parse_block(inner)?);
                }
            }
            
            Ok(Expr::Quote {
                code: code.unwrap(),
                span: Span::default(),
            })
        }
        Rule::regex_literal => {
            // Phase 4G: Parse regex literal
            parse_regex_literal(pair)
        }
        Rule::format_string => {
            // Phase 4G: Parse format string
            parse_format_string(pair)
        }
        _ => {
            // Handle parenthesized expressions
            if pair.as_rule() == Rule::expr {
                return parse_expr(pair);
            }
            unreachable!("Unexpected rule: {:?}", pair.as_rule())
        }
    }
}

fn parse_postfix_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut inner_pairs = pair.into_inner();
    let mut expr = parse_expr_primary(inner_pairs.next().unwrap())?;
    
    for postfix in inner_pairs {
        match postfix.as_rule() {
            Rule::field_access => {
                let ident = parse_ident(postfix.into_inner().next().unwrap());
                expr = Expr::FieldAccess {
                    expr: Box::new(expr),
                    field: ident,
                    span: Span::default(),
                };
            }
            Rule::index_access => {
                let index_expr = parse_expr(postfix.into_inner().next().unwrap())?;
                
                // Phase 4G: Check if this is a range expression for string slicing
                if let Expr::Range { start, end, step, .. } = &index_expr {
                    expr = Expr::StringSlice {
                        string: Box::new(expr),
                        range: SliceRange {
                            start: start.clone(),
                            end: end.clone(),
                            step: step.clone(),
                        },
                        span: Span::default(),
                    };
                } else {
                    expr = Expr::Index {
                        expr: Box::new(expr),
                        index: Box::new(index_expr),
                        span: Span::default(),
                    };
                }
            }
            Rule::optional_chain => {
                let ident = parse_ident(postfix.into_inner().next().unwrap());
                expr = Expr::OptionalChain {
                    expr: Box::new(expr),
                    field: ident,
                    span: Span::default(),
                };
            }
            Rule::method_call => {
                let mut args = Vec::new();
                for arg_pair in postfix.into_inner() {
                    if arg_pair.as_rule() == Rule::call_args {
                        args = parse_call_args(arg_pair)?;
                    }
                }
                
                // Phase 4C: Check if any arguments are placeholders for partial application
                let has_placeholder = args.iter().any(|(_, arg)| matches!(arg, Expr::Placeholder(_)));
                
                if has_placeholder {
                    // Convert to partial application
                    let partial_args = args.into_iter().map(|(_, arg)| {
                        if matches!(arg, Expr::Placeholder(_)) {
                            crate::ast::PartialArg::Placeholder
                        } else {
                            crate::ast::PartialArg::Fixed(arg)
                        }
                    }).collect();
                    
                    expr = Expr::PartialApplication {
                        func: Box::new(expr),
                        args: partial_args,
                        span: Span::default(),
                    };
                } else {
                    expr = Expr::Call {
                        func: Box::new(expr),
                        args,
                        span: Span::default(),
                    };
                }
            }
            Rule::await_expr => {
                expr = Expr::Await(Box::new(expr), Span::default());
            }
            Rule::try_operator => {
                // Phase 4E: Question mark operator
                expr = Expr::TryOperator {
                    expr: Box::new(expr),
                    span: Span::default(),
                };
            }
            _ => {}
        }
    }
    
    Ok(expr)
}

fn parse_call_args(pair: Pair<Rule>) -> Result<Vec<(Option<Ident>, Expr)>, pest::error::Error<Rule>> {
    let mut args = Vec::new();
    
    for call_arg in pair.into_inner() {
        if call_arg.as_rule() == Rule::call_arg {
            let mut name = None;
            let mut expr = None;
            
            for inner in call_arg.into_inner() {
                match inner.as_rule() {
                    Rule::ident => {
                        name = Some(parse_ident(inner));
                    }
                    Rule::expr => {
                        expr = Some(parse_expr(inner)?);
                    }
                    _ => {}
                }
            }
            
            args.push((name, expr.unwrap()));
        }
    }
    
    Ok(args)
}

fn parse_if_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut condition = None;
    let mut then_branch = None;
    let mut else_branch = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::expr => {
                if condition.is_none() {
                    condition = Some(Box::new(parse_expr(inner)?));
                }
            }
            Rule::block => {
                if then_branch.is_none() {
                    then_branch = Some(parse_block(inner)?);
                } else {
                    else_branch = Some(parse_block(inner)?);
                }
            }
            Rule::expr_if => {
                // Else-if chain
                let else_if_expr = parse_if_expr(inner)?;
                else_branch = Some(Block {
                    stmts: Vec::new(),
                    expr: Some(Box::new(else_if_expr)),
                    span: Span::default(),
                });
            }
            _ => {}
        }
    }
    
    Ok(Expr::If {
        condition: condition.unwrap(),
        then_branch: then_branch.unwrap(),
        else_branch,
        span: Span::default(),
    })
}

fn parse_match_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut expr = None;
    let mut arms = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::expr => {
                expr = Some(Box::new(parse_expr(inner)?));
            }
            Rule::match_arm => {
                arms.push(parse_match_arm(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Expr::Match {
        expr: expr.unwrap(),
        arms,
        span: Span::default(),
    })
}

fn parse_match_arm(pair: Pair<Rule>) -> Result<MatchArm, pest::error::Error<Rule>> {
    let mut pattern = None;
    let mut guard = None;
    let mut body = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::pattern => {
                pattern = Some(parse_pattern(inner)?);
            }
            Rule::expr => {
                if guard.is_none() && pattern.is_some() {
                    guard = Some(parse_expr(inner)?);
                }
            }
            Rule::match_arm_body => {
                let body_inner = inner.into_inner().next().unwrap();
                body = Some(match body_inner.as_rule() {
                    Rule::block => Expr::Block(parse_block(body_inner)?),
                    Rule::expr_primary => parse_expr_primary(body_inner)?,
                    _ => unreachable!(),
                });
            }
            _ => {}
        }
    }
    
    Ok(MatchArm {
        pattern: pattern.unwrap(),
        guard,
        body: body.unwrap(),
    })
}

fn parse_struct_init(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut name = None;
    let mut fields = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::struct_field => {
                let mut field_name = None;
                let mut field_expr = None;
                
                for field_inner in inner.into_inner() {
                    match field_inner.as_rule() {
                        Rule::ident => {
                            field_name = Some(parse_ident(field_inner));
                        }
                        Rule::expr => {
                            field_expr = Some(parse_expr(field_inner)?);
                        }
                        _ => {}
                    }
                }
                
                fields.push((field_name.unwrap(), field_expr.unwrap()));
            }
            _ => {}
        }
    }
    
    Ok(Expr::StructInit {
        name: name.unwrap(),
        fields,
        span: Span::default(),
    })
}

fn parse_list_comp(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut expr = None;
    let mut pattern = None;
    let mut iter = None;
    let mut condition = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::expr => {
                if expr.is_none() {
                    expr = Some(Box::new(parse_expr(inner)?));
                } else if iter.is_none() {
                    iter = Some(Box::new(parse_expr(inner)?));
                } else {
                    condition = Some(Box::new(parse_expr(inner)?));
                }
            }
            Rule::pattern => {
                pattern = Some(parse_pattern(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(Expr::ListComprehension {
        expr: expr.unwrap(),
        pattern: pattern.unwrap(),
        iter: iter.unwrap(),
        condition,
        span: Span::default(),
    })
}

fn parse_literal(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    let literal = match inner.as_rule() {
        Rule::number => {
            let num: f64 = inner.as_str().parse().unwrap();
            Literal::Number(num)
        }
        Rule::string => {
            let s = inner.as_str();
            let s = &s[1..s.len()-1]; // Remove quotes
            Literal::String(s.into())
        }
        Rule::boolean => {
            let b = inner.as_str() == "true";
            Literal::Boolean(b)
        }
        Rule::array_literal => {
            let mut elements = Vec::new();
            for elem in inner.into_inner() {
                if elem.as_rule() == Rule::expr {
                    elements.push(parse_expr(elem)?);
                }
            }
            Literal::Array(elements)
        }
        Rule::tuple_literal => {
            let mut elements = Vec::new();
            for elem in inner.into_inner() {
                if elem.as_rule() == Rule::expr {
                    elements.push(parse_expr(elem)?);
                }
            }
            Literal::Tuple(elements)
        }
        Rule::regex_literal => {
            // Parse regex literal - delegate to parse_regex_literal
            return parse_regex_literal(inner);
        }
        _ => unreachable!("Unexpected literal rule: {:?}", inner.as_rule()),
    };
    
    Ok(Expr::Literal(literal, Span::default()))
}

fn parse_pattern(pair: Pair<Rule>) -> Result<Pattern, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::pattern_wildcard => Ok(Pattern::Wildcard(Span::default())),
        Rule::pattern_ident => {
            let ident = parse_ident(inner.into_inner().next().unwrap());
            Ok(Pattern::Ident(ident))
        }
        Rule::pattern_literal => {
            let lit_pair = inner.into_inner().next().unwrap();
            let literal = match lit_pair.as_rule() {
                Rule::number => {
                    let num: f64 = lit_pair.as_str().parse().unwrap();
                    Literal::Number(num)
                }
                Rule::string => {
                    let s = lit_pair.as_str();
                    let s = &s[1..s.len()-1];
                    Literal::String(s.into())
                }
                Rule::boolean => {
                    let b = lit_pair.as_str() == "true";
                    Literal::Boolean(b)
                }
                _ => unreachable!(),
            };
            Ok(Pattern::Literal(literal))
        }
        Rule::pattern_tuple => {
            let mut patterns = Vec::new();
            for pat_inner in inner.into_inner() {
                if pat_inner.as_rule() == Rule::pattern {
                    patterns.push(parse_pattern(pat_inner)?);
                }
            }
            Ok(Pattern::Tuple(patterns))
        }
        Rule::pattern_record => {
            let mut fields = Vec::new();
            for field_inner in inner.into_inner() {
                if field_inner.as_rule() == Rule::pattern_field {
                    let mut name = None;
                    let mut pattern = None;
                    
                    for field_part in field_inner.into_inner() {
                        match field_part.as_rule() {
                            Rule::ident => {
                                name = Some(parse_ident(field_part));
                            }
                            Rule::pattern => {
                                pattern = Some(parse_pattern(field_part)?);
                            }
                            _ => {}
                        }
                    }
                    
                    fields.push((name.unwrap(), pattern.unwrap()));
                }
            }
            Ok(Pattern::Record(fields))
        }
        _ => unreachable!(),
    }
}

fn parse_type(pair: Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::type_union => {
            // Phase 4B: Parse union types (A | B | C)
            let mut types = Vec::new();
            for union_inner in inner.into_inner() {
                if union_inner.as_rule() == Rule::type_intersection {
                    types.push(parse_type_intersection(union_inner)?);
                }
            }
            if types.len() == 1 {
                Ok(types.into_iter().next().unwrap())
            } else {
                Ok(crate::ast::Type::Union(types))
            }
        }
        // Fallback for old code paths - parse as type_primary
        _ => {
            parse_type_primary(inner)
        }
    }
}

fn parse_ident(pair: Pair<Rule>) -> Ident {
    let span_range = pair.as_span();
    Ident {
        name: pair.as_str().into(),
        span: Span {
            start: span_range.start(),
            end: span_range.end(),
            file_id: 0,
        },
    }
}

// Phase 4A: Parse const expressions for const generics
fn parse_const_expr(pair: Pair<Rule>) -> Result<crate::ast::ConstExpr, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::const_binary => {
            let mut parts = inner.into_inner();
            let first = parts.next().unwrap();
            let mut left = parse_const_primary(first)?;
            
            while let Some(op_pair) = parts.next() {
                let op = match op_pair.as_str() {
                    "+" => crate::ast::BinaryOp::Add,
                    "-" => crate::ast::BinaryOp::Sub,
                    "*" => crate::ast::BinaryOp::Mul,
                    "/" => crate::ast::BinaryOp::Div,
                    _ => crate::ast::BinaryOp::Add,
                };
                
                if let Some(right_pair) = parts.next() {
                    let right = parse_const_primary(right_pair)?;
                    left = crate::ast::ConstExpr::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    };
                }
            }
            
            Ok(left)
        }
        Rule::const_primary => parse_const_primary(inner),
        _ => parse_const_primary(inner),
    }
}

fn parse_const_primary(pair: Pair<Rule>) -> Result<crate::ast::ConstExpr, pest::error::Error<Rule>> {
    match pair.as_rule() {
        Rule::number => {
            let n = pair.as_str().parse::<f64>().unwrap();
            Ok(crate::ast::ConstExpr::Number(n))
        }
        Rule::ident => {
            Ok(crate::ast::ConstExpr::Param(parse_ident(pair)))
        }
        Rule::const_primary => {
            let inner = pair.into_inner().next().unwrap();
            parse_const_primary(inner)
        }
        _ => {
            // Try to parse as number or ident
            if let Ok(n) = pair.as_str().parse::<f64>() {
                Ok(crate::ast::ConstExpr::Number(n))
            } else {
                Ok(crate::ast::ConstExpr::Param(parse_ident(pair)))
            }
        }
    }
}

// Phase 4A: Parse effect handler
fn parse_effect_handler(pair: Pair<Rule>) -> Result<crate::ast::EffectHandler, pest::error::Error<Rule>> {
    let mut effect = None;
    let mut params = Vec::new();
    let mut body = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if effect.is_none() {
                    effect = Some(parse_ident(inner));
                }
            }
            Rule::pattern => {
                params.push(parse_pattern(inner)?);
            }
            Rule::block => {
                body = Some(parse_block(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::EffectHandler {
        effect: effect.unwrap(),
        params,
        body: body.unwrap(),
        span: Span::default(),
    })
}

// Phase 4A: Parse WebAssembly instruction
fn parse_wasm_instruction(pair: Pair<Rule>) -> Result<crate::ast::WasmInstruction, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::wasm_local_get => {
            let num = inner.into_inner().next().unwrap().as_str().parse::<u32>().unwrap();
            Ok(crate::ast::WasmInstruction::LocalGet(num))
        }
        Rule::wasm_local_set => {
            let num = inner.into_inner().next().unwrap().as_str().parse::<u32>().unwrap();
            Ok(crate::ast::WasmInstruction::LocalSet(num))
        }
        Rule::wasm_const => {
            let text = inner.as_str();
            if text.starts_with("i32.const") {
                let num = inner.into_inner().next().unwrap().as_str().parse::<i32>().unwrap();
                Ok(crate::ast::WasmInstruction::I32Const(num))
            } else if text.starts_with("i64.const") {
                let num = inner.into_inner().next().unwrap().as_str().parse::<i64>().unwrap();
                Ok(crate::ast::WasmInstruction::I64Const(num))
            } else if text.starts_with("f32.const") {
                let num = inner.into_inner().next().unwrap().as_str().parse::<f32>().unwrap();
                Ok(crate::ast::WasmInstruction::F32Const(num))
            } else {
                let num = inner.into_inner().next().unwrap().as_str().parse::<f64>().unwrap();
                Ok(crate::ast::WasmInstruction::F64Const(num))
            }
        }
        Rule::wasm_arithmetic => {
            match inner.as_str() {
                "i32.add" => Ok(crate::ast::WasmInstruction::I32Add),
                "i32.sub" => Ok(crate::ast::WasmInstruction::I32Sub),
                "i32.mul" => Ok(crate::ast::WasmInstruction::I32Mul),
                "f64.add" => Ok(crate::ast::WasmInstruction::F64Add),
                "f64.sub" => Ok(crate::ast::WasmInstruction::F64Sub),
                "f64.mul" => Ok(crate::ast::WasmInstruction::F64Mul),
                "f64.div" => Ok(crate::ast::WasmInstruction::F64Div),
                _ => Ok(crate::ast::WasmInstruction::Raw(inner.as_str().to_string())),
            }
        }
        Rule::wasm_comparison => {
            match inner.as_str() {
                "i32.eq" => Ok(crate::ast::WasmInstruction::I32Eq),
                "i32.ne" => Ok(crate::ast::WasmInstruction::I32Ne),
                "f64.eq" => Ok(crate::ast::WasmInstruction::F64Eq),
                "f64.ne" => Ok(crate::ast::WasmInstruction::F64Ne),
                "f64.lt" => Ok(crate::ast::WasmInstruction::F64Lt),
                "f64.gt" => Ok(crate::ast::WasmInstruction::F64Gt),
                _ => Ok(crate::ast::WasmInstruction::Raw(inner.as_str().to_string())),
            }
        }
        Rule::wasm_control => {
            let text = inner.as_str();
            if text == "drop" {
                Ok(crate::ast::WasmInstruction::Drop)
            } else if text == "return" {
                Ok(crate::ast::WasmInstruction::Return)
            } else if text.starts_with("call") {
                let num = inner.into_inner().next().unwrap().as_str().parse::<u32>().unwrap();
                Ok(crate::ast::WasmInstruction::Call(num))
            } else {
                Ok(crate::ast::WasmInstruction::Raw(text.to_string()))
            }
        }
        Rule::wasm_raw => {
            Ok(crate::ast::WasmInstruction::Raw(inner.as_str().to_string()))
        }
        _ => Ok(crate::ast::WasmInstruction::Raw(inner.as_str().to_string())),
    }
}

// Phase 4B: Parse intersection types (A & B & C)
fn parse_type_intersection(pair: Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
    let mut types = Vec::new();
    
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::type_primary {
            types.push(parse_type_primary(inner)?);
        }
    }
    
    if types.len() == 1 {
        Ok(types.into_iter().next().unwrap())
    } else {
        Ok(crate::ast::Type::Intersection(types))
    }
}

// Phase 4B: Parse primary type expressions
fn parse_type_primary(pair: Pair<Rule>) -> Result<Type, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_rule() {
        Rule::type_reference => {
            // Parse reference types (&'a T or &mut T)
            let mut mutable = false;
            let mut inner_type = None;
            let mut lifetime = None;
            
            for ref_inner in inner.into_inner() {
                match ref_inner.as_rule() {
                    Rule::lifetime => {
                        lifetime = Some(parse_lifetime(ref_inner)?);
                    }
                    Rule::type_expr => {
                        inner_type = Some(parse_type(ref_inner)?);
                    }
                    _ => {
                        if ref_inner.as_str() == "mut" {
                            mutable = true;
                        }
                    }
                }
            }
            
            let inner = inner_type.unwrap();
            if let Some(lt) = lifetime {
                Ok(Type::reference_with_lifetime(inner, mutable, lt))
            } else {
                Ok(Type::reference(inner, mutable))
            }
        }
        Rule::type_const_array => {
            // Parse const generic array [T; N]
            let mut elem_type = None;
            let mut size_expr = None;
            
            for array_inner in inner.into_inner() {
                match array_inner.as_rule() {
                    Rule::type_expr => {
                        elem_type = Some(parse_type(array_inner)?);
                    }
                    Rule::const_expr => {
                        size_expr = Some(parse_const_expr(array_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Type::ConstArray {
                element_type: Box::new(elem_type.unwrap()),
                size: size_expr.unwrap(),
            })
        }
        Rule::type_refinement => {
            // Phase 4B: Parse refinement types {x: T | predicate}
            let mut binder = None;
            let mut base_type = None;
            let mut predicate = None;
            
            for ref_inner in inner.into_inner() {
                match ref_inner.as_rule() {
                    Rule::ident => {
                        if binder.is_none() {
                            binder = Some(parse_ident(ref_inner));
                        }
                    }
                    Rule::type_expr => {
                        base_type = Some(parse_type(ref_inner)?);
                    }
                    Rule::expr => {
                        predicate = Some(parse_expr(ref_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Type::Refinement {
                base: Box::new(base_type.unwrap()),
                binder: binder.unwrap(),
                predicate: Box::new(predicate.unwrap()),
            })
        }
        Rule::type_dependent => {
            // Phase 4B: Parse dependent types Type(value1, value2, ...)
            let mut constructor = None;
            let mut value_params = Vec::new();
            
            for dep_inner in inner.into_inner() {
                match dep_inner.as_rule() {
                    Rule::ident => {
                        if constructor.is_none() {
                            constructor = Some(parse_ident(dep_inner));
                        }
                    }
                    Rule::expr => {
                        value_params.push(parse_expr(dep_inner)?);
                    }
                    _ => {}
                }
            }
            
            Ok(Type::Dependent {
                constructor: constructor.unwrap(),
                value_params,
            })
        }
        Rule::type_existential => {
            // Phase 4B: Parse existential types impl Trait + Trait2
            let mut bounds = Vec::new();
            for exist_inner in inner.into_inner() {
                if exist_inner.as_rule() == Rule::ident {
                    bounds.push(parse_ident(exist_inner));
                }
            }
            Ok(Type::Existential { bounds })
        }
        Rule::type_level_lit => {
            // Phase 4B: Parse type-level literal #42
            let lit_str = inner.as_str();
            let num_str = &lit_str[1..]; // Skip the '#'
            let num: i32 = num_str.parse().unwrap();
            Ok(Type::TypeLevelLit(num))
        }
        Rule::type_phantom_data => {
            // Phase 4B: Parse PhantomData<T>
            let inner_type = parse_type(inner.into_inner().next().unwrap())?;
            Ok(Type::PhantomData(Box::new(inner_type)))
        }
        Rule::type_immutable_vec => {
            // Phase 4C: Parse ImmutableVec<T>
            let elem_type = parse_type(inner.into_inner().next().unwrap())?;
            Ok(Type::ImmutableVec(Box::new(elem_type)))
        }
        Rule::type_immutable_map => {
            // Phase 4C: Parse ImmutableMap<K, V>
            let mut types = inner.into_inner();
            let key_type = parse_type(types.next().unwrap())?;
            let value_type = parse_type(types.next().unwrap())?;
            Ok(Type::ImmutableMap {
                key_type: Box::new(key_type),
                value_type: Box::new(value_type),
            })
        }
        Rule::type_immutable_set => {
            // Phase 4C: Parse ImmutableSet<T>
            let elem_type = parse_type(inner.into_inner().next().unwrap())?;
            Ok(Type::ImmutableSet(Box::new(elem_type)))
        }
        Rule::type_array => {
            let elem_type = parse_type(inner.into_inner().next().unwrap())?;
            Ok(Type::Array(Box::new(elem_type)))
        }
        Rule::type_tuple => {
            let mut types = Vec::new();
            for type_inner in inner.into_inner() {
                if type_inner.as_rule() == Rule::type_expr {
                    types.push(parse_type(type_inner)?);
                }
            }
            Ok(Type::Tuple(types))
        }
        Rule::type_function => {
            let mut params = Vec::new();
            let mut return_type = None;
            
            for type_inner in inner.into_inner() {
                if type_inner.as_rule() == Rule::type_expr {
                    if return_type.is_none() {
                        params.push(parse_type(type_inner)?);
                    } else {
                        return_type = Some(parse_type(type_inner)?);
                    }
                }
            }
            
            Ok(Type::Function {
                params,
                return_type: Box::new(return_type.unwrap_or(Type::Infer)),
            })
        }
        Rule::type_higher_kinded => {
            // Phase 4B: Parse higher-kinded types F<_> or F<T>
            let mut constructor = None;
            let mut args = Vec::new();
            let mut has_wildcard = false;
            
            for hk_inner in inner.into_inner() {
                match hk_inner.as_rule() {
                    Rule::ident => {
                        constructor = Some(parse_ident(hk_inner));
                    }
                    Rule::type_hk_args => {
                        for arg_inner in hk_inner.into_inner() {
                            if arg_inner.as_rule() == Rule::type_hk_arg {
                                let arg_content = arg_inner.into_inner().next().unwrap();
                                if arg_content.as_str() == "_" {
                                    has_wildcard = true;
                                    args.push(Type::Infer);
                                } else {
                                    args.push(parse_type(arg_content)?);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            
            let constructor_ident = constructor.unwrap();
            
            if has_wildcard && args.iter().all(|t| matches!(t, Type::Infer)) {
                // Pure higher-kinded type: F<_> or F<_, _>
                Ok(Type::HigherKinded {
                    constructor: constructor_ident,
                    arity: args.len(),
                })
            } else {
                // Applied higher-kinded type: F<T> or F<i32, String>
                Ok(Type::AppliedHigherKinded {
                    constructor: Box::new(Type::Generic(constructor_ident)),
                    args,
                })
            }
        }
        Rule::type_generic => {
            let ident = parse_ident(inner.into_inner().next().unwrap());
            // Map built-in type names to their corresponding Type variants
            match ident.name.as_ref() {
                "number" => Ok(Type::Number),
                "string" => Ok(Type::String),
                "boolean" => Ok(Type::Boolean),
                _ => Ok(Type::Generic(ident))
            }
        }
        Rule::type_expr => {
            // Parenthesized type
            parse_type(inner)
        }
        _ => {
            // Fallback to generic
            Ok(Type::Generic(crate::ast::Ident {
                name: inner.as_str().into(),
                span: Span::default(),
            }))
        }
    }
}

// Phase 4B: Parse type alias
fn parse_type_alias(pair: Pair<Rule>) -> Result<crate::ast::TypeAlias, pest::error::Error<Rule>> {
    let mut name = None;
    let mut type_params = Vec::new();
    let mut target = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::type_params => {
                for param in inner.into_inner() {
                    if param.as_rule() == Rule::ident {
                        type_params.push(parse_ident(param));
                    }
                }
            }
            Rule::type_expr => {
                target = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::TypeAlias {
        name: name.unwrap(),
        type_params,
        target: target.unwrap(),
        span: Span::default(),
    })
}

// Phase 4B: Parse type-level function
fn parse_type_function(pair: Pair<Rule>) -> Result<crate::ast::TypeFunction, pest::error::Error<Rule>> {
    let mut name = None;
    let mut params = Vec::new();
    let mut body = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                } else {
                    params.push(parse_ident(inner));
                }
            }
            Rule::type_expr => {
                body = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::TypeFunction {
        name: name.unwrap(),
        params,
        body: body.unwrap(),
        span: Span::default(),
    })
}

// Phase 4B: Parse associated type
fn parse_associated_type(pair: Pair<Rule>) -> Result<crate::ast::AssociatedType, pest::error::Error<Rule>> {
    let mut name = None;
    let mut bounds = Vec::new();
    let mut default = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                } else {
                    // This is a bound - convert Ident to TypeBound::Trait
                    bounds.push(crate::ast::TypeBound::Trait(parse_ident(inner)));
                }
            }
            Rule::type_expr => {
                default = Some(parse_type(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::AssociatedType {
        name: name.unwrap(),
        bounds,
        default,
    })
}

// Phase 4D: Parse select arm
fn parse_select_arm(pair: Pair<Rule>) -> Result<crate::ast::SelectArm, pest::error::Error<Rule>> {
    let mut pattern = None;
    let mut body = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::select_pattern => {
                pattern = Some(parse_select_pattern(inner)?);
            }
            Rule::block => {
                body = Some(parse_block(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::SelectArm {
        pattern: pattern.unwrap(),
        body: body.unwrap(),
    })
}

// Phase 4D: Parse select pattern
fn parse_select_pattern(pair: Pair<Rule>) -> Result<crate::ast::SelectPattern, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    
    match inner.as_str() {
        "_" => Ok(crate::ast::SelectPattern::Default),
        _ => {
            // Parse recv, send, or timeout patterns
            // For now, return a default pattern
            // TODO: Implement full pattern parsing
            Ok(crate::ast::SelectPattern::Default)
        }
    }
}

// Phase 4D: Parse atomic operation
fn parse_atomic_op(pair: Pair<Rule>) -> Result<crate::ast::AtomicOp, pest::error::Error<Rule>> {
    match pair.as_str() {
        "load" => Ok(crate::ast::AtomicOp::Load),
        "store" => Ok(crate::ast::AtomicOp::Store),
        "swap" => Ok(crate::ast::AtomicOp::Swap),
        "compare_exchange" => Ok(crate::ast::AtomicOp::CompareExchange),
        "fetch_add" => Ok(crate::ast::AtomicOp::FetchAdd),
        "fetch_sub" => Ok(crate::ast::AtomicOp::FetchSub),
        "fetch_and" => Ok(crate::ast::AtomicOp::FetchAnd),
        "fetch_or" => Ok(crate::ast::AtomicOp::FetchOr),
        "fetch_xor" => Ok(crate::ast::AtomicOp::FetchXor),
        _ => unreachable!("Unknown atomic operation: {}", pair.as_str()),
    }
}

// Phase 4D: Parse memory ordering
fn parse_memory_ordering(pair: Pair<Rule>) -> Result<crate::ast::MemoryOrdering, pest::error::Error<Rule>> {
    match pair.as_str() {
        "Relaxed" => Ok(crate::ast::MemoryOrdering::Relaxed),
        "Acquire" => Ok(crate::ast::MemoryOrdering::Acquire),
        "Release" => Ok(crate::ast::MemoryOrdering::Release),
        "AcqRel" => Ok(crate::ast::MemoryOrdering::AcqRel),
        "SeqCst" => Ok(crate::ast::MemoryOrdering::SeqCst),
        _ => unreachable!("Unknown memory ordering: {}", pair.as_str()),
    }
}

// Phase 4E: Parse catch clause
fn parse_catch_clause(pair: Pair<Rule>) -> Result<crate::ast::CatchClause, pest::error::Error<Rule>> {
    let mut error_type = None;
    let mut binding = None;
    let mut body = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if error_type.is_none() {
                    error_type = Some(parse_ident(inner));
                } else {
                    binding = Some(parse_ident(inner));
                }
            }
            Rule::block => {
                body = Some(parse_block(inner)?);
            }
            _ => {}
        }
    }
    
    Ok(crate::ast::CatchClause {
        error_type,
        binding,
        body: body.unwrap(),
    })
}

// Phase 4E: Parse catch expression
fn parse_catch_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut parts = pair.into_inner();
    let first = parts.next().unwrap();
    let mut expr = parse_expr_hierarchy(first)?;
    
    if let Some(handler_pair) = parts.next() {
        let handler = parse_expr_hierarchy(handler_pair)?;
        expr = Expr::Catch {
            expr: Box::new(expr),
            handler: Box::new(handler),
            span: Span::default(),
        };
    }
    
    Ok(expr)
}

// Phase 4E: Parse cfg condition
fn parse_cfg_condition(pair: Pair<Rule>) -> Result<crate::ast::CfgCondition, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    let text = inner.as_str();
    
    if text.starts_with("target") {
        let parts: Vec<&str> = text.split('=').collect();
        if parts.len() == 2 {
            return Ok(crate::ast::CfgCondition::Target(parts[1].trim_matches('"').to_string()));
        }
    } else if text.starts_with("feature") {
        let parts: Vec<&str> = text.split('=').collect();
        if parts.len() == 2 {
            return Ok(crate::ast::CfgCondition::Feature(parts[1].trim_matches('"').to_string()));
        }
    }
    
    match text {
        "debug" => Ok(crate::ast::CfgCondition::Debug),
        "release" => Ok(crate::ast::CfgCondition::Release),
        "test" => Ok(crate::ast::CfgCondition::Test),
        _ => Ok(crate::ast::CfgCondition::Debug), // Default
    }
}

// Phase 4F: Parse attribute (derive or custom)
fn parse_attribute(pair: Pair<Rule>) -> Result<(Option<crate::ast::DeriveAttribute>, Option<crate::ast::CustomAttribute>), pest::error::Error<Rule>> {
    let content = pair.into_inner().next().unwrap();
    let text = content.as_str();
    
    if text.starts_with("derive") {
        let mut traits = Vec::new();
        for inner in content.into_inner() {
            if inner.as_rule() == Rule::ident {
                traits.push(parse_ident(inner));
            }
        }
        Ok((Some(crate::ast::DeriveAttribute {
            traits,
            span: Span::default(),
        }), None))
    } else {
        let mut name = None;
        let mut args = Vec::new();
        for inner in content.into_inner() {
            match inner.as_rule() {
                Rule::ident => {
                    if name.is_none() {
                        name = Some(parse_ident(inner));
                    }
                }
                Rule::expr => {
                    args.push(parse_expr(inner)?);
                }
                _ => {}
            }
        }
        if let Some(n) = name {
            Ok((None, Some(crate::ast::CustomAttribute {
                name: n,
                args,
                span: Span::default(),
            })))
        } else {
            Ok((None, None))
        }
    }
}


// Phase 4G: Parse regex literal
fn parse_regex_literal(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut pattern = String::new();
    let mut flags = String::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::regex_pattern => {
                pattern = inner.as_str().to_string();
            }
            Rule::regex_flags => {
                flags = inner.as_str().to_string();
            }
            _ => {}
        }
    }
    
    // Combine pattern and flags into a single regex string
    let regex_str = if flags.is_empty() {
        pattern
    } else {
        format!("(?{}){}", flags, pattern)
    };
    
    Ok(Expr::Literal(
        Literal::Regex(regex_str.into()),
        Span::default(),
    ))
}

// Phase 4G: Parse format string
fn parse_format_string(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut parts = Vec::new();
    
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::string_with_format {
            for format_part in inner.into_inner() {
                match format_part.as_rule() {
                    Rule::format_part => {
                        let part_inner = format_part.into_inner().next().unwrap();
                        match part_inner.as_rule() {
                            Rule::format_text => {
                                parts.push(FormatPart::Text(part_inner.as_str().into()));
                            }
                            Rule::format_expr => {
                                let mut expr = None;
                                let mut format_spec = None;
                                
                                for item in part_inner.into_inner() {
                                    match item.as_rule() {
                                        Rule::expr => {
                                            expr = Some(Box::new(parse_expr(item)?));
                                        }
                                        Rule::format_spec => {
                                            format_spec = Some(parse_format_spec(item)?);
                                        }
                                        _ => {}
                                    }
                                }
                                
                                parts.push(FormatPart::Formatted {
                                    expr: expr.unwrap(),
                                    format_spec,
                                });
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    
    Ok(Expr::FormatString {
        parts,
        span: Span::default(),
    })
}

// Phase 4G: Parse format specification
fn parse_format_spec(pair: Pair<Rule>) -> Result<FormatSpec, pest::error::Error<Rule>> {
    let fill = None;
    let mut align = None;
    let mut sign = None;
    let mut width = None;
    let mut precision = None;
    let mut type_spec = None;
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::format_align => {
                align = Some(match inner.as_str() {
                    "<" => Alignment::Left,
                    ">" => Alignment::Right,
                    "^" => Alignment::Center,
                    _ => Alignment::Left,
                });
            }
            Rule::format_sign => {
                sign = Some(match inner.as_str() {
                    "+" => Sign::Plus,
                    "-" => Sign::Minus,
                    " " => Sign::Space,
                    _ => Sign::Minus,
                });
            }
            Rule::format_width => {
                width = inner.as_str().parse().ok();
            }
            Rule::format_precision => {
                precision = inner.as_str().parse().ok();
            }
            Rule::format_type => {
                type_spec = Some(inner.as_str().into());
            }
            _ => {}
        }
    }
    
    Ok(FormatSpec {
        fill,
        align,
        sign,
        width,
        precision,
        type_spec,
    })
}

// Phase 4G: Parse string slice (handled in postfix)
#[allow(dead_code)]
fn parse_string_slice(base_expr: Expr, pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let mut start = None;
    let mut end = None;
    let mut step = None;
    
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::slice_range {
            let range_str = inner.as_str();
            
            // Parse range syntax: start..end or start..end::step
            if range_str.contains("::") {
                // Has step
                let parts: Vec<&str> = range_str.split("::").collect();
                let range_part = parts[0];
                let step_part = parts[1];
                
                // Parse step
                if !step_part.is_empty() {
                    step = Some(Box::new(Expr::Literal(
                        Literal::Number(step_part.parse().unwrap_or(1.0)),
                        Span::default(),
                    )));
                }
                
                // Parse start..end
                if range_part.contains("..") {
                    let range_parts: Vec<&str> = range_part.split("..").collect();
                    if !range_parts[0].is_empty() {
                        start = Some(Box::new(Expr::Literal(
                            Literal::Number(range_parts[0].parse().unwrap_or(0.0)),
                            Span::default(),
                        )));
                    }
                    if range_parts.len() > 1 && !range_parts[1].is_empty() {
                        end = Some(Box::new(Expr::Literal(
                            Literal::Number(range_parts[1].parse().unwrap_or(0.0)),
                            Span::default(),
                        )));
                    }
                }
            } else if range_str.contains("..") {
                // Just start..end
                let parts: Vec<&str> = range_str.split("..").collect();
                if !parts[0].is_empty() {
                    start = Some(Box::new(Expr::Literal(
                        Literal::Number(parts[0].parse().unwrap_or(0.0)),
                        Span::default(),
                    )));
                }
                if parts.len() > 1 && !parts[1].is_empty() {
                    end = Some(Box::new(Expr::Literal(
                        Literal::Number(parts[1].parse().unwrap_or(0.0)),
                        Span::default(),
                    )));
                }
            } else {
                // Single index
                start = Some(Box::new(Expr::Literal(
                    Literal::Number(range_str.parse().unwrap_or(0.0)),
                    Span::default(),
                )));
                end = start.clone();
            }
        }
    }
    
    Ok(Expr::StringSlice {
        string: Box::new(base_expr),
        range: SliceRange { start, end, step },
        span: Span::default(),
    })
}
