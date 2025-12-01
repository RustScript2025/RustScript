

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
        Rule::extend_block => parse_extend_block(inner),
        Rule::import => Ok(Item::Import(parse_import(inner)?)),
        _ => unreachable!(),
    }
}

fn parse_function(pair: Pair<Rule>) -> Result<Function, pest::error::Error<Rule>> {
    let mut async_ = false;
    let mut generator = false;
    let mut is_pure = false;
    let mut name = None;
    let mut params = Vec::new();
    let mut return_type = None;
    let mut contracts = Vec::new();
    let mut body = None;
    
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
            Rule::contract => {
                contracts.push(parse_contract(inner)?);
            }
            Rule::block => {
                body = Some(parse_block(inner)?);
            }
            _ => {
                // Handle modifiers (async, gen, pure)
                match inner.as_str() {
                    "async" => async_ = true,
                    "gen" => generator = true,
                    "pure" => is_pure = true,
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
        span: Span::default(),
    })
}

fn parse_params(pair: Pair<Rule>) -> Result<Vec<(Pattern, Option<Type>)>, pest::error::Error<Rule>> {
    let mut params = Vec::new();
    
    for param_pair in pair.into_inner() {
        if param_pair.as_rule() == Rule::param {
            let mut pattern = None;
            let mut type_ann = None;
            
            for inner in param_pair.into_inner() {
                match inner.as_rule() {
                    Rule::pattern => {
                        pattern = Some(parse_pattern(inner)?);
                    }
                    Rule::type_expr => {
                        type_ann = Some(parse_type(inner)?);
                    }
                    _ => {}
                }
            }
            
            params.push((pattern.unwrap(), type_ann));
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
    let mut name = None;
    let mut fields = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => {
                if name.is_none() {
                    name = Some(parse_ident(inner));
                }
            }
            Rule::struct_field_def => {
                let mut field_name = None;
                let mut field_type = None;
                
                for field_inner in inner.into_inner() {
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
        span: Span::default(),
    })
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
        Rule::expr_pipeline => parse_binary_chain(pair, BinaryOp::Pipeline),
        Rule::expr_null_coalesce => parse_null_coalesce_chain(pair),
        Rule::expr_logical_or => parse_binary_chain(pair, BinaryOp::Or),
        Rule::expr_logical_and => parse_binary_chain(pair, BinaryOp::And),
        Rule::expr_comparison => parse_comparison_chain(pair),
        Rule::expr_additive => parse_additive_chain(pair),
        Rule::expr_multiplicative => parse_multiplicative_chain(pair),
        Rule::expr_postfix => parse_postfix_expr(pair),
        Rule::expr_primary => parse_expr_primary(pair),
        _ => parse_expr_primary(pair),
    }
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
        Rule::expr_postfix => {
            parse_postfix_expr(pair)
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
                expr = Expr::Index {
                    expr: Box::new(expr),
                    index: Box::new(index_expr),
                    span: Span::default(),
                };
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
                expr = Expr::Call {
                    func: Box::new(expr),
                    args,
                    span: Span::default(),
                };
            }
            Rule::await_expr => {
                expr = Expr::Await(Box::new(expr), Span::default());
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
        _ => unreachable!(),
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
        Rule::type_generic => {
            let ident = parse_ident(inner.into_inner().next().unwrap());
            Ok(Type::Generic(ident))
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
                        return_type = Some(Box::new(parse_type(type_inner)?));
                    }
                }
            }
            
            Ok(Type::Function {
                params,
                return_type: return_type.unwrap_or_else(|| Box::new(Type::Tuple(vec![]))),
            })
        }
        _ => unreachable!(),
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
