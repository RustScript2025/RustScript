//! WebAssembly Code Generator for RustScript.
//!
//! Author: Michael Lauzon
//!
//! This module generates WebAssembly bytecode from a RustScript AST. It handles
//! the translation of all language constructs into WASM instructions, including
//! functions, control flow, expressions, and memory management.
//!
//! The generator uses the `wasm-encoder` crate to produce valid WASM binary format.
//! It supports:
//! - Function definitions and calls
//! - Struct allocation and field access
//! - String literals (interned in linear memory)
//! - Control flow (if/else, loops, match expressions)
//! - Binary operations and comparisons
//! - Console output via imported JavaScript functions

use wasm_encoder::{
    CodeSection, DataSection, ExportKind, ExportSection, Function, FunctionSection, 
    Instruction, Module, TypeSection, ValType, ImportSection, EntityType, MemoryType,
    MemorySection, ConstExpr, DataSegment, DataSegmentMode
};
use crate::ast::{self, Type};
use std::collections::HashMap;

/// Generates WebAssembly bytecode from a RustScript AST.
///
/// The generator maintains state for building a complete WASM module, including
/// type definitions, imports, function bodies, and data segments for string literals.
///
/// # Architecture
///
/// The WASM module is built incrementally:
/// 1. Import console.log, console.error, and malloc from the host environment
/// 2. Process struct definitions to calculate memory layouts
/// 3. Process function definitions, generating WASM instructions for each
/// 4. Assemble all sections into the final binary
pub struct WasmGenerator {
    /// The WASM module being constructed.
    module: Module,
    /// Type section containing function signatures.
    types: TypeSection,
    /// Import section for host functions (console, malloc).
    imports: ImportSection,
    /// Function section mapping functions to their type indices.
    funcs: FunctionSection,
    /// Export section for functions accessible from JavaScript.
    exports: ExportSection,
    /// Code section containing function bodies.
    codes: CodeSection,
    /// Data section for string literals and static data.
    data: DataSection,
    /// Memory section defining linear memory.
    memory: MemorySection,

    /// Interned string literals: string content -> (offset, length).
    string_literals: HashMap<String, (u32, u32)>,
    /// Next available offset in the data segment.
    next_data_offset: u32,

    /// Maps function names to their indices in the module.
    func_map: HashMap<String, u32>,
    /// Maps imported function names to their indices.
    import_map: HashMap<String, u32>,

    /// Count of user-defined functions (used to calculate indices).
    func_count: u32,

    /// Number of imported functions (offsets user function indices).
    num_imports: u32,

    /// Struct layouts: name -> (total_size, field_offsets).
    struct_map: HashMap<String, (u32, HashMap<String, (u32, ValType)>)>,
    /// Expression types from the type checker (used for field access).
    expr_types: HashMap<ast::Span, Type>,
    /// Counter for type indices.
    type_count: u32,
}

impl WasmGenerator {
    /// Creates a new WASM generator with standard imports configured.
    ///
    /// The generator is initialised with imports for:
    /// - `console.log(ptr, len)` - Output a string to the console
    /// - `console.error(ptr, len)` - Output an error message
    /// - `malloc(size)` - Allocate memory on the heap
    pub fn new() -> Self {
        let mut imports = ImportSection::new();
        let mut import_map = HashMap::new();

        // Import console.log: takes pointer and length, returns nothing.
        imports.import("console", "log", EntityType::Function(0));
        import_map.insert("console.log".to_string(), 0);

        // Import console.error: same signature as console.log.
        imports.import("console", "error", EntityType::Function(0));
        import_map.insert("console.error".to_string(), 1);

        // Import malloc: takes size, returns pointer.
        imports.import("env", "malloc", EntityType::Function(1));
        import_map.insert("malloc".to_string(), 2);

        let num_imports = 3u32;

        Self {
            module: Module::new(),
            types: TypeSection::new(),
            imports,
            funcs: FunctionSection::new(),
            exports: ExportSection::new(),
            codes: CodeSection::new(),
            data: DataSection::new(),
            memory: MemorySection::new(),
            string_literals: HashMap::new(),
            next_data_offset: 0,
            func_map: HashMap::new(),
            import_map,
            func_count: 0,
            num_imports,
            struct_map: HashMap::new(),
            expr_types: HashMap::new(),
            type_count: 0,
        }
    }

    /// Adds a function type signature to the type section.
    ///
    /// Returns the index of the newly added type, which is used when
    /// declaring functions that have this signature.
    fn add_function_type(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        self.types.ty().function(params, results);
        let idx = self.type_count;
        self.type_count += 1;
        idx
    }

    /// Generates a complete WASM binary from the AST.
    ///
    /// This is the main entry point for code generation. It processes all
    /// items in the module and assembles them into a valid WASM binary.
    ///
    /// # Arguments
    ///
    /// * `ast_module` - The parsed AST to generate code from
    /// * `expr_types` - Type information from the type checker
    ///
    /// # Returns
    ///
    /// The WASM binary as a byte vector, or an error message.
    pub fn generate(mut self, ast_module: &ast::Module, expr_types: HashMap<ast::Span, Type>) -> Result<Vec<u8>, String> {
        self.expr_types = expr_types;
        
        // Type 0: (i32, i32) -> () for console.log
        self.add_function_type(vec![ValType::I32, ValType::I32], vec![]);
        // Type 1: (i32) -> (i32) for malloc
        self.add_function_type(vec![ValType::I32], vec![ValType::I32]);
        
        self.memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        self.exports.export("memory", ExportKind::Memory, 0);

        for item in &ast_module.items {
            match item {
                ast::Item::Function(f) => self.process_function(f)?,
                ast::Item::Struct(s) => self.process_struct(s)?,
                ast::Item::Extend { methods, .. } => {
                    for method in methods {
                        self.process_function(method)?;
                    }
                },
                ast::Item::Import(_) | ast::Item::MultiFn { .. } | ast::Item::Comptime { .. } => {},
            }
        }

        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.funcs);
        self.module.section(&self.memory);
        self.module.section(&self.exports);
        self.module.section(&self.codes);
        self.module.section(&self.data);

        Ok(self.module.finish())
    }

    /// Processes a function definition and generates its WASM code.
    ///
    /// This method:
    /// 1. Creates a type signature for the function
    /// 2. Registers the function in the module
    /// 3. Exports the function if it's named "main"
    /// 4. Analyses local variables needed
    /// 5. Generates WASM instructions for the function body
    fn process_function(&mut self, func_def: &ast::Function) -> Result<(), String> {
        // Map RustScript parameter types to WASM value types.
        let params: Vec<ValType> = func_def.params.iter()
            .map(|(_, ty)| self.map_type(ty.as_ref()))
            .collect();

        // Map return type (empty vector if void).
        let results: Vec<ValType> = func_def.return_type.as_ref()
            .map(|ty| vec![self.map_type(Some(ty))])
            .unwrap_or_default();

        // Register the function's type signature.
        let type_idx = self.add_function_type(params.clone(), results.clone());

        // Calculate function index (imports come first, then user functions).
        let func_idx = self.num_imports + self.func_count;
        self.func_count += 1;

        self.funcs.function(type_idx);
        self.func_map.insert(func_def.name.name.to_string(), func_idx);

        // The main function is exported so JavaScript can call it.
        if func_def.name.name.as_ref() == "main" {
            self.exports.export("main", ExportKind::Func, func_idx);
        }

        // Analyse local variables and create the function body.
        // The scratch local (index 0) is used for temporary values.
        let (locals_map, local_types) = self.analyze_locals(&func_def.body);
        let mut func_body = Function::new(local_types.iter().map(|&vt| (1, vt)));
        let scratch_local = params.len() as u32;
        
        for stmt in &func_def.body.stmts {
            self.generate_stmt(&mut func_body, stmt, scratch_local, &locals_map)?;
        }
        
        if let Some(expr) = &func_def.body.expr {
            self.generate_expr(&mut func_body, expr, scratch_local, &locals_map)?;
        } else if !results.is_empty() {
            match results[0] {
                ValType::I32 => { func_body.instruction(&Instruction::I32Const(0)); },
                ValType::F64 => { func_body.instruction(&Instruction::F64Const(0.0)); },
                _ => { func_body.instruction(&Instruction::I32Const(0)); },
            };
        }
        
        func_body.instruction(&Instruction::End);
        self.codes.function(&func_body);

        Ok(())
    }

    /// Generates WASM instructions for a statement.
    ///
    /// Statements include variable declarations, expression statements,
    /// returns, guards, and defers.
    fn generate_stmt(&mut self, func: &mut Function, stmt: &ast::Stmt, scratch_local: u32, locals: &HashMap<String, u32>) -> Result<(), String> {
        match stmt {
            ast::Stmt::Let { pattern, value, .. } => {
                if let ast::Pattern::Ident(id) = pattern {
                    if let Some(&idx) = locals.get(id.name.as_ref()) {
                        if let Some(val_expr) = value {
                            self.generate_expr(func, val_expr, scratch_local, locals)?;
                        } else {
                            func.instruction(&Instruction::F64Const(0.0));
                        }
                        let local_idx = scratch_local + 1 + idx;
                        func.instruction(&Instruction::LocalSet(local_idx));
                    }
                }
            },
            ast::Stmt::Expr(expr, _) => {
                self.generate_expr(func, expr, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
            },
            ast::Stmt::Return(expr, _) => {
                if let Some(ret_expr) = expr {
                    self.generate_expr(func, ret_expr, scratch_local, locals)?;
                }
                func.instruction(&Instruction::Return);
            },
            ast::Stmt::Guard { condition, else_block, .. } => {
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                for inner_stmt in &else_block.stmts {
                    self.generate_stmt(func, inner_stmt, scratch_local, locals)?;
                }
                func.instruction(&Instruction::End);
            },
            ast::Stmt::Defer { .. } => {},
        }
        Ok(())
    }

    /// Generates WASM instructions for an expression.
    ///
    /// Expressions leave their result on the WASM stack. The type of value
    /// depends on the expression (f64 for numbers, i32 for booleans and pointers).
    fn generate_expr(&mut self, func: &mut Function, expr: &ast::Expr, scratch_local: u32, locals: &HashMap<String, u32>) -> Result<(), String> {
        match expr {
            ast::Expr::Literal(lit, _) => {
                match lit {
                    ast::Literal::Number(n) => {
                        func.instruction(&Instruction::F64Const(*n));
                    },
                    ast::Literal::String(s) => {
                        let (ptr, len) = self.intern_string(s);
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    },
                    ast::Literal::Boolean(b) => {
                        func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                    },
                    _ => {
                        func.instruction(&Instruction::I32Const(0));
                    }
                }
            },
            ast::Expr::Ident(id) => {
                if let Some(&idx) = locals.get(id.name.as_ref()) {
                    func.instruction(&Instruction::LocalGet(scratch_local + 1 + idx));
                } else {
                    return Err(format!("Unknown variable: {}", id.name));
                }
            },
            ast::Expr::Call { func: callee, args, .. } => {
                // Handle console.log and console.error
                if let ast::Expr::FieldAccess { expr: obj, field, .. } = callee.as_ref() {
                    if let ast::Expr::Ident(obj_id) = obj.as_ref() {
                        if obj_id.name.as_ref() == "console" {
                            let func_name = format!("console.{}", field.name);
                            if let Some(&idx) = self.import_map.get(&func_name) {
                                for (_, arg) in args {
                                    self.generate_expr(func, arg, scratch_local, locals)?;
                                }
                                func.instruction(&Instruction::Call(idx));
                                func.instruction(&Instruction::I32Const(0));
                                return Ok(());
                            }
                        }
                    }
                }
                
                // Handle regular function calls
                if let ast::Expr::Ident(fn_id) = callee.as_ref() {
                    if let Some(&idx) = self.func_map.get(fn_id.name.as_ref()) {
                        for (_, arg) in args {
                            self.generate_expr(func, arg, scratch_local, locals)?;
                        }
                        func.instruction(&Instruction::Call(idx));
                        return Ok(());
                    }
                }
                
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Binary { left, op, right, .. } => {
                if *op == ast::BinaryOp::Pipeline {
                    self.generate_expr(func, left, scratch_local, locals)?;
                    if let ast::Expr::Ident(id) = right.as_ref() {
                        if let Some(&idx) = self.func_map.get(id.name.as_ref()) {
                            func.instruction(&Instruction::Call(idx));
                        }
                    }
                } else {
                    self.generate_expr(func, left, scratch_local, locals)?;
                    self.generate_expr(func, right, scratch_local, locals)?;
                    match op {
                        ast::BinaryOp::Add => { func.instruction(&Instruction::F64Add); },
                        ast::BinaryOp::Sub => { func.instruction(&Instruction::F64Sub); },
                        ast::BinaryOp::Mul => { func.instruction(&Instruction::F64Mul); },
                        ast::BinaryOp::Div => { func.instruction(&Instruction::F64Div); },
                        ast::BinaryOp::Eq => { func.instruction(&Instruction::F64Eq); },
                        ast::BinaryOp::Neq => {
                            func.instruction(&Instruction::F64Eq);
                            func.instruction(&Instruction::I32Eqz);
                        },
                        ast::BinaryOp::Lt => { func.instruction(&Instruction::F64Lt); },
                        ast::BinaryOp::Gt => { func.instruction(&Instruction::F64Gt); },
                        ast::BinaryOp::Leq => { func.instruction(&Instruction::F64Le); },
                        ast::BinaryOp::Geq => { func.instruction(&Instruction::F64Ge); },
                        ast::BinaryOp::And => { func.instruction(&Instruction::I32And); },
                        ast::BinaryOp::Or => { func.instruction(&Instruction::I32Or); },
                        ast::BinaryOp::Pipeline => {},
                    };
                }
            },
            ast::Expr::If { condition, then_branch, else_branch, .. } => {
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                for stmt in &then_branch.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &then_branch.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                if let Some(else_block) = else_branch {
                    func.instruction(&Instruction::Else);
                    for stmt in &else_block.stmts {
                        self.generate_stmt(func, stmt, scratch_local, locals)?;
                    }
                    if let Some(expr) = &else_block.expr {
                        self.generate_expr(func, expr, scratch_local, locals)?;
                        func.instruction(&Instruction::Drop);
                    }
                }
                func.instruction(&Instruction::End);
            },
            ast::Expr::Loop { body, .. } => {
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End);
            },
            ast::Expr::While { condition, body, .. } => {
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::BrIf(1));
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::End);
            },
            ast::Expr::StructInit { name, fields, .. } => {
                let (total_size, field_offsets) = self.struct_map.get(name.name.as_ref())
                    .cloned()
                    .ok_or_else(|| format!("Unknown struct: {}", name.name))?;
                func.instruction(&Instruction::I32Const(total_size as i32));
                if let Some(&malloc_idx) = self.import_map.get("malloc") {
                    func.instruction(&Instruction::Call(malloc_idx));
                }
                func.instruction(&Instruction::LocalSet(scratch_local));
                for (field_name, field_expr) in fields {
                    let (offset, val_type) = field_offsets.get(field_name.name.as_ref())
                        .ok_or_else(|| format!("Unknown field: {}", field_name.name))?;
                    func.instruction(&Instruction::LocalGet(scratch_local));
                    self.generate_expr(func, field_expr, scratch_local, locals)?;
                    match val_type {
                        ValType::I32 => { func.instruction(&Instruction::I32Store(wasm_encoder::MemArg { offset: *offset as u64, align: 2, memory_index: 0 })); },
                        ValType::F64 => { func.instruction(&Instruction::F64Store(wasm_encoder::MemArg { offset: *offset as u64, align: 3, memory_index: 0 })); },
                        _ => {},
                    };
                }
                func.instruction(&Instruction::LocalGet(scratch_local));
            },
            ast::Expr::FieldAccess { expr: obj, field, .. } => {
                self.generate_expr(func, obj, scratch_local, locals)?;
                if let Some(expr_type) = self.expr_types.get(obj.span()) {
                    if let Type::Generic(struct_id) = expr_type {
                        if let Some((_, offsets)) = self.struct_map.get(struct_id.name.as_ref()) {
                            if let Some((offset, val_type)) = offsets.get(field.name.as_ref()) {
                                match val_type {
                                    ValType::I32 => { func.instruction(&Instruction::I32Load(wasm_encoder::MemArg { offset: *offset as u64, align: 2, memory_index: 0 })); },
                                    ValType::F64 => { func.instruction(&Instruction::F64Load(wasm_encoder::MemArg { offset: *offset as u64, align: 3, memory_index: 0 })); },
                                    _ => {},
                                }
                            }
                        }
                    }
                }
            },
            ast::Expr::OptionalChain { expr: obj, field, .. } => {
                self.generate_expr(func, obj, scratch_local, locals)?;
                if let Some(expr_type) = self.expr_types.get(obj.span()) {
                    if let Type::Generic(struct_id) = expr_type {
                        if let Some((_, offsets)) = self.struct_map.get(struct_id.name.as_ref()) {
                            if let Some((offset, val_type)) = offsets.get(field.name.as_ref()) {
                                match val_type {
                                    ValType::I32 => { func.instruction(&Instruction::I32Load(wasm_encoder::MemArg { offset: *offset as u64, align: 2, memory_index: 0 })); },
                                    ValType::F64 => { func.instruction(&Instruction::F64Load(wasm_encoder::MemArg { offset: *offset as u64, align: 3, memory_index: 0 })); },
                                    _ => {},
                                }
                            }
                        }
                    }
                }
            },
            ast::Expr::NullCoalesce { left, right, .. } => {
                self.generate_expr(func, left, scratch_local, locals)?;
                func.instruction(&Instruction::LocalTee(scratch_local));
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                self.generate_expr(func, right, scratch_local, locals)?;
                func.instruction(&Instruction::LocalSet(scratch_local));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::LocalGet(scratch_local));
            },
            ast::Expr::Match { expr: match_expr, arms, .. } => {
                self.generate_expr(func, match_expr, scratch_local, locals)?;
                func.instruction(&Instruction::LocalSet(scratch_local));
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                for arm in arms {
                    match &arm.pattern {
                        ast::Pattern::Literal(lit) => {
                            func.instruction(&Instruction::LocalGet(scratch_local));
                            match lit {
                                ast::Literal::Number(n) => {
                                    func.instruction(&Instruction::F64Const(*n));
                                    func.instruction(&Instruction::F64Eq);
                                },
                                ast::Literal::Boolean(b) => {
                                    func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                                    func.instruction(&Instruction::I32Eq);
                                },
                                _ => {
                                    func.instruction(&Instruction::Drop);
                                    func.instruction(&Instruction::I32Const(0));
                                }
                            }
                            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                            self.generate_expr(func, &arm.body, scratch_local, locals)?;
                            func.instruction(&Instruction::Drop);
                            func.instruction(&Instruction::Br(1));
                            func.instruction(&Instruction::End);
                        },
                        ast::Pattern::Wildcard(_) | ast::Pattern::Ident(_) => {
                            self.generate_expr(func, &arm.body, scratch_local, locals)?;
                            func.instruction(&Instruction::Drop);
                            func.instruction(&Instruction::Br(0));
                        },
                        _ => {}
                    }
                }
                func.instruction(&Instruction::End);
            },
            ast::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &block.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Pipeline { steps, .. } => {
                if let Some(first) = steps.first() {
                    self.generate_expr(func, first, scratch_local, locals)?;
                    for step in steps.iter().skip(1) {
                        if let ast::Expr::Ident(id) = step {
                            if let Some(&idx) = self.func_map.get(id.name.as_ref()) {
                                func.instruction(&Instruction::Call(idx));
                            }
                        }
                    }
                }
            },
            ast::Expr::Async(inner, _) | ast::Expr::Await(inner, _) => {
                self.generate_expr(func, inner, scratch_local, locals)?;
            },
            ast::Expr::Yield { value, .. } => {
                if let Some(val) = value {
                    self.generate_expr(func, val, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Comptime { block, .. } => {
                for stmt in &block.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &block.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::ContractExpr { condition, .. } => {
                self.generate_expr(func, condition, scratch_local, locals)?;
            },
            ast::Expr::ListComprehension { .. } => {
                func.instruction(&Instruction::I32Const(0));
            },
        }
        Ok(())
    }

    /// Analyses a block to determine what local variables are needed.
    ///
    /// Returns a map from variable names to local indices, and a list of
    /// WASM value types for each local. The first local is always a scratch
    /// variable used for temporary storage.
    fn analyze_locals(&self, block: &ast::Block) -> (HashMap<String, u32>, Vec<ValType>) {
        let mut locals_map = HashMap::new();
        // Reserve local 0 as a scratch variable for intermediate values.
        let mut local_types = vec![ValType::I32];
        self.collect_locals(block, &mut locals_map, &mut local_types);
        (locals_map, local_types)
    }

    /// Recursively collects local variable declarations from a block.
    fn collect_locals(&self, block: &ast::Block, locals_map: &mut HashMap<String, u32>, local_types: &mut Vec<ValType>) {
        for stmt in &block.stmts {
            if let ast::Stmt::Let { pattern, type_ann, .. } = stmt {
                if let ast::Pattern::Ident(id) = pattern {
                    let idx = (local_types.len() - 1) as u32;
                    locals_map.insert(id.name.to_string(), idx);
                    local_types.push(self.map_type(type_ann.as_ref()));
                }
            }
        }
    }

    /// Maps a RustScript type to a WASM value type.
    ///
    /// Numbers are represented as f64, whilst booleans, strings, arrays,
    /// and other reference types are represented as i32 pointers.
    fn map_type(&self, ty: Option<&Type>) -> ValType {
        match ty {
            Some(Type::Number) => ValType::F64,
            Some(Type::Boolean) => ValType::I32,
            Some(Type::String) => ValType::I32,  // Pointer to string data.
            Some(Type::Array(_)) => ValType::I32,  // Pointer to array.
            Some(Type::Tuple(_)) => ValType::I32,  // Pointer to tuple.
            Some(Type::Record(_)) => ValType::I32,  // Pointer to record.
            Some(Type::Generic(_)) => ValType::I32,  // Pointer to struct instance.
            Some(Type::Function { .. }) => ValType::I32,  // Function reference.
            Some(Type::Infer) | None => ValType::F64,  // Default to number.
        }
    }

    /// Interns a string literal in the data section.
    ///
    /// Returns the offset and length of the string in linear memory.
    /// Duplicate strings are deduplicated to save space.
    fn intern_string(&mut self, s: &str) -> (u32, u32) {
        if let Some(&loc) = self.string_literals.get(s) {
            return loc;
        }
        
        let offset = self.next_data_offset;
        let bytes: Vec<u8> = s.as_bytes().to_vec();
        let len = bytes.len() as u32;
        
        let offset_expr = ConstExpr::i32_const(offset as i32);
        let segment = DataSegment {
            mode: DataSegmentMode::Active {
                memory_index: 0,
                offset: &offset_expr,
            },
            data: bytes,
        };
        self.data.segment(segment);
        
        self.next_data_offset += len;
        self.string_literals.insert(s.to_string(), (offset, len));
        (offset, len)
    }

    /// Processes a struct definition and calculates its memory layout.
    ///
    /// Fields are laid out sequentially with appropriate alignment:
    /// - f64 fields are 8-byte aligned
    /// - i32 fields are 4-byte aligned
    ///
    /// The total struct size is padded to 8-byte alignment.
    fn process_struct(&mut self, s: &ast::Struct) -> Result<(), String> {
        let mut field_offsets = HashMap::new();
        let mut current_offset = 0u32;

        for (name, ty) in &s.fields {
            let val_type = self.map_type(Some(ty));
            // Align the field to its natural alignment.
            let align = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
            while current_offset % align != 0 { current_offset += 1; }
            field_offsets.insert(name.name.to_string(), (current_offset, val_type));
            // Advance by the field's size.
            let size = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
            current_offset += size;
        }
        // Pad the struct to 8-byte alignment for consistent allocation.
        while current_offset % 8 != 0 { current_offset += 1; }
        self.struct_map.insert(s.name.name.to_string(), (current_offset, field_offsets));
        Ok(())
    }
}
