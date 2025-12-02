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
    /// Phase 4G: Trait implementations: (trait_name, type_name) -> TraitImpl
    trait_impls: HashMap<(String, String), ast::TraitImpl>,
    /// Phase 4G: Parameter types for current function (variable_name -> Type)
    param_types: HashMap<String, Type>,
    /// Phase 4G: Current trait impl context (trait_name, for_type) for inferring self type
    current_trait_impl: Option<(String, Type)>,
    /// Phase 4G: Function return types for type inference
    func_return_types: HashMap<String, ValType>,
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

        // Import input: takes prompt pointer and length, returns string pointer.
        // Type 2: (i32, i32) -> (i32) - takes ptr/len, returns result pointer.
        imports.import("env", "input", EntityType::Function(2));
        import_map.insert("input".to_string(), 3);

        let num_imports = 4u32;

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
            trait_impls: HashMap::new(),
            param_types: HashMap::new(),
            current_trait_impl: None,
            func_return_types: HashMap::new(),
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
        // Type 2: (i32, i32) -> (i32) for input
        self.add_function_type(vec![ValType::I32, ValType::I32], vec![ValType::I32]);
        
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
                ast::Item::TraitImpl(trait_impl) => {
                    // Phase 4G: Store trait implementation for operator overloading
                    let trait_name = trait_impl.trait_name.name.to_string();
                    let type_name = self.type_to_string(&trait_impl.for_type);
                    self.trait_impls.insert((trait_name.clone(), type_name), trait_impl.clone());
                    
                    // Set current trait impl context for self type inference
                    self.current_trait_impl = Some((trait_name, trait_impl.for_type.clone()));
                    
                    // Process trait methods
                    for method in &trait_impl.methods {
                        self.process_function(method)?;
                    }
                    
                    // Clear trait impl context
                    self.current_trait_impl = None;
                },
                ast::Item::Enum(e) => {
                    // Phase 4B: Process GADT enum definition
                    self.process_enum(e)?;
                },
                ast::Item::Import(_) | ast::Item::MultiFn { .. } | ast::Item::Comptime { .. } | ast::Item::Trait(_) | ast::Item::TypeAlias(_) | ast::Item::TypeFunction(_) => {
                    // Phase 4B: Type aliases and type functions are compile-time only
                },
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
        // Infer types for parameters without annotations (e.g., self in trait impls)
        let params: Vec<ValType> = func_def.params.iter()
            .map(|(pattern, ty, _default)| {
                if let Some(param_ty) = ty {
                    self.map_type(Some(param_ty))
                } else if let ast::Pattern::Ident(id) = pattern {
                    // Infer type for self parameter in trait impls
                    if id.name.as_ref() == "self" || id.name.as_ref() == "other" {
                        if let Some((_trait_name, for_type)) = &self.current_trait_impl {
                            self.map_type(Some(for_type))
                        } else {
                            self.map_type(None)
                        }
                    } else {
                        self.map_type(None)
                    }
                } else {
                    self.map_type(None)
                }
            })
            .collect();

        // Map return type (empty vector if void).
        let results: Vec<ValType> = func_def.return_type.as_ref()
            .map(|ty| vec![self.map_type(Some(ty))])
            .unwrap_or_default();
        
        // Track return type for type inference
        if let Some(ret_val_type) = results.first() {
            self.func_return_types.insert(func_def.name.name.to_string(), *ret_val_type);
        }

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
        // Parameters come first (indices 0, 1, 2, ...), then locals
        let (locals_map, local_types) = self.analyze_locals_with_params(&func_def.params, &func_def.body);
        
        // Clear and populate parameter types for this function
        self.param_types.clear();
        for (pattern, ty, _default) in &func_def.params {
            if let ast::Pattern::Ident(id) = pattern {
                let param_ty = if let Some(t) = ty {
                    t.clone()
                } else if id.name.as_ref() == "self" || id.name.as_ref() == "other" {
                    // Infer type for self/other parameter in trait impls
                    if let Some((_trait_name, for_type)) = &self.current_trait_impl {
                        for_type.clone()
                    } else {
                        continue;
                    }
                } else {
                    continue;
                };
                self.param_types.insert(id.name.to_string(), param_ty);
            }
        }
        
        let mut func_body = Function::new(local_types.iter().map(|&vt| (1, vt)));
        let scratch_local = params.len() as u32;
        
        // Phase 4A: Tail call optimization - wrap body in loop for tail-recursive functions
        if func_def.tail_call_optimized {
            func_body.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        }
        
        for stmt in &func_def.body.stmts {
            self.generate_stmt(&mut func_body, stmt, scratch_local, &locals_map)?;
        }
        
        if let Some(expr) = &func_def.body.expr {
            self.generate_expr(&mut func_body, expr, scratch_local, &locals_map)?;
        } else if !results.is_empty() {
            match results[0] {
                ValType::I32 => { func_body.instruction(&Instruction::I32Const(0)); },
                ValType::F64 => { func_body.instruction(&Instruction::F64Const(0.0_f64.into())); },
                _ => { func_body.instruction(&Instruction::I32Const(0)); },
            };
        }
        
        // Phase 4A: Close the loop for tail-optimized functions
        if func_def.tail_call_optimized {
            func_body.instruction(&Instruction::End); // End loop
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
                match pattern {
                    ast::Pattern::Ident(id) => {
                        if let Some(&idx) = locals.get(id.name.as_ref()) {
                            if let Some(val_expr) = value {
                                self.generate_expr(func, val_expr, scratch_local, locals)?;
                            } else {
                                func.instruction(&Instruction::F64Const(0.0_f64.into()));
                            }
                            // Phase 4G: idx is already the correct WASM local index
                            func.instruction(&Instruction::LocalSet(idx));
                        }
                    }
                    ast::Pattern::Tuple(patterns) => {
                        // Phase 4G: Tuple destructuring
                        // Generate the tuple expression
                        if let Some(val_expr) = value {
                            // For tuple destructuring, we need to:
                            // 1. Evaluate the tuple expression (returns a pointer to tuple in memory)
                            // 2. Load each element from the tuple
                            // 3. Store into the corresponding local variables
                            
                            self.generate_expr(func, val_expr, scratch_local, locals)?;
                            
                            // Store tuple pointer in scratch local
                            func.instruction(&Instruction::LocalSet(scratch_local));
                            
                            // Extract each element from the tuple
                            for (idx, pat) in patterns.iter().enumerate() {
                                if let ast::Pattern::Ident(id) = pat {
                                    if let Some(&local_idx) = locals.get(id.name.as_ref()) {
                                        // Load tuple pointer
                                        func.instruction(&Instruction::LocalGet(scratch_local));
                                        
                                        // Load element at offset (idx * 8 for f64 elements)
                                        // Assuming all tuple elements are f64 for now
                                        let offset = (idx * 8) as u64;
                                        func.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
                                            offset,
                                            align: 3,
                                            memory_index: 0,
                                        }));
                                        
                                        // Store in local variable
                                        func.instruction(&Instruction::LocalSet(local_idx));
                                    }
                                }
                            }
                        }
                    }
                    ast::Pattern::Record(fields) => {
                        // Phase 4G: Struct destructuring
                        if let Some(val_expr) = value {
                            // Generate the struct expression (returns pointer)
                            self.generate_expr(func, val_expr, scratch_local, locals)?;
                            
                            // Store struct pointer
                            func.instruction(&Instruction::LocalSet(scratch_local));
                            
                            // Get the struct type to know field offsets
                            // This requires type information from the expression
                            // For now, we'll need to look up the struct layout
                            
                            for (_field_name, field_pattern) in fields {
                                if let ast::Pattern::Ident(id) = field_pattern {
                                    if let Some(&local_idx) = locals.get(id.name.as_ref()) {
                                        // We need to know the struct type to get field offsets
                                        // This would require type inference or annotations
                                        // For now, this is a placeholder that needs proper type info
                                        func.instruction(&Instruction::LocalGet(scratch_local));
                                        // TODO: Load field at proper offset based on struct layout
                                        func.instruction(&Instruction::F64Const(0.0_f64.into()));
                                        func.instruction(&Instruction::LocalSet(local_idx));
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        // Other patterns not yet supported
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
            ast::Stmt::Break { value, .. } => {
                // Phase 4E: Break statement
                if let Some(v) = value {
                    self.generate_expr(func, v, scratch_local, locals)?;
                }
                func.instruction(&Instruction::Br(0));
            },
            ast::Stmt::Continue { .. } => {
                // Phase 4E: Continue statement
                func.instruction(&Instruction::Br(1));
            },
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
                        func.instruction(&Instruction::F64Const((*n).into()));
                    },
                    ast::Literal::String(s) => {
                        let (ptr, len) = self.intern_string(s);
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    },
                    ast::Literal::Boolean(b) => {
                        func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                    },
                    ast::Literal::Regex(pattern) => {
                        // Phase 4G: Regex literal - store as string for now
                        let (ptr, len) = self.intern_string(pattern);
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    },
                    _ => {
                        func.instruction(&Instruction::I32Const(0));
                    }
                }
            },
            ast::Expr::Ident(id) => {
                if let Some(&idx) = locals.get(id.name.as_ref()) {
                    // Phase 4G: idx is already the correct WASM local index
                    // (parameters are 0, 1, 2, ..., then scratch, then user locals)
                    func.instruction(&Instruction::LocalGet(idx));
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
                
                // Handle input() built-in function.
                if let ast::Expr::Ident(fn_id) = callee.as_ref() {
                    if fn_id.name.as_ref() == "input" {
                        if let Some(&idx) = self.import_map.get("input") {
                            // Pass the prompt string if provided.
                            if let Some((_, arg)) = args.first() {
                                self.generate_expr(func, arg, scratch_local, locals)?;
                            } else {
                                // No prompt - pass empty string.
                                let (ptr, len) = self.intern_string("");
                                func.instruction(&Instruction::I32Const(ptr as i32));
                                func.instruction(&Instruction::I32Const(len as i32));
                            }
                            func.instruction(&Instruction::Call(idx));
                            // Convert i32 pointer to f64 for storage in untyped locals.
                            // The pointer value is preserved as a float for later use.
                            func.instruction(&Instruction::F64ConvertI32U);
                            return Ok(());
                        }
                    }
                }
                
                // Handle regular function calls.
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
                    // Phase 4G: Check for operator overloading via traits
                    let trait_method = self.get_trait_method_for_op(left, op)?;
                    
                    if let Some((method_name, _type_name)) = trait_method {
                        // Call trait method instead of built-in operation
                        self.generate_expr(func, left, scratch_local, locals)?;
                        self.generate_expr(func, right, scratch_local, locals)?;
                        
                        // Look up the method function
                        if let Some(&method_idx) = self.func_map.get(&method_name) {
                            func.instruction(&Instruction::Call(method_idx));
                        } else {
                            return Err(format!("Trait method not found: {}", method_name));
                        }
                    } else {
                        // Use built-in operations
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
                            ast::BinaryOp::ComposeForward | ast::BinaryOp::ComposeBackward => {
                                // Phase 4C: Function composition creates a new function
                                // For now, treat as function call composition
                            },
                            ast::BinaryOp::ApplicativeFmap | ast::BinaryOp::ApplicativeApply => {
                                // Phase 4C: Applicative functors
                                // For now, treat as function application
                            },
                        };
                    }
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
                // Use a dedicated local for the struct pointer to avoid conflicts with scratch_local
                let struct_ptr_local = scratch_local + 1;
                func.instruction(&Instruction::LocalSet(struct_ptr_local));
                for (field_name, field_expr) in fields {
                    let (offset, val_type) = field_offsets.get(field_name.name.as_ref())
                        .ok_or_else(|| format!("Unknown field: {}", field_name.name))?;
                    func.instruction(&Instruction::LocalGet(struct_ptr_local));
                    self.generate_expr(func, field_expr, scratch_local, locals)?;
                    match val_type {
                        ValType::I32 => { func.instruction(&Instruction::I32Store(wasm_encoder::MemArg { offset: *offset as u64, align: 2, memory_index: 0 })); },
                        ValType::F64 => { func.instruction(&Instruction::F64Store(wasm_encoder::MemArg { offset: *offset as u64, align: 3, memory_index: 0 })); },
                        _ => {},
                    };
                }
                func.instruction(&Instruction::LocalGet(struct_ptr_local));
            },
            ast::Expr::FieldAccess { expr: obj, field, .. } => {
                self.generate_expr(func, obj, scratch_local, locals)?;
                
                // Try to get the type from expr_types first, then from param_types if obj is an identifier
                let obj_type = self.expr_types.get(obj.span()).or_else(|| {
                    if let ast::Expr::Ident(id) = obj.as_ref() {
                        self.param_types.get(id.name.as_ref())
                    } else {
                        None
                    }
                });
                
                if let Some(expr_type) = obj_type {
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
            ast::Expr::Index { expr: arr, index, .. } => {
                // Phase 4G: Custom indexing via Index trait
                // Get the type of the indexed expression
                let arr_type = self.expr_types.get(arr.span()).or_else(|| {
                    if let ast::Expr::Ident(id) = arr.as_ref() {
                        self.param_types.get(id.name.as_ref())
                    } else {
                        None
                    }
                });
                
                if let Some(Type::Generic(type_id)) = arr_type {
                    // Look up Index trait implementation for this type
                    let method_name = self.trait_impls.get(&("Index".to_string(), type_id.name.to_string()))
                        .and_then(|trait_impl| {
                            trait_impl.methods.iter().find(|f| {
                                f.name.name.as_ref() == "index"
                            }).map(|f| f.name.name.to_string())
                        });
                    
                    if let Some(method_name) = method_name {
                        // Generate call to index method: obj.index(idx)
                        // Push arguments: self, index
                        self.generate_expr(func, arr, scratch_local, locals)?;
                        self.generate_expr(func, index, scratch_local, locals)?;
                        
                        // Call the index method
                        if let Some(&method_idx) = self.func_map.get(&method_name) {
                            func.instruction(&Instruction::Call(method_idx));
                            return Ok(());
                        }
                    }
                }
                
                // Fallback: basic array indexing (for built-in arrays)
                // This would require proper array support in memory
                self.generate_expr(func, arr, scratch_local, locals)?;
                self.generate_expr(func, index, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
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
                                    func.instruction(&Instruction::F64Const((*n).into()));
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
                            
                            // Phase 4A: Pattern guards - check guard condition if present
                            if let Some(guard_expr) = &arm.guard {
                                // Pattern matched, now check guard
                                func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
                                self.generate_expr(func, guard_expr, scratch_local, locals)?;
                                func.instruction(&Instruction::Else);
                                func.instruction(&Instruction::I32Const(0));
                                func.instruction(&Instruction::End);
                            }
                            
                            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                            self.generate_expr(func, &arm.body, scratch_local, locals)?;
                            func.instruction(&Instruction::Drop);
                            func.instruction(&Instruction::Br(1));
                            func.instruction(&Instruction::End);
                        },
                        ast::Pattern::Wildcard(_) | ast::Pattern::Ident(_) => {
                            // Phase 4A: Pattern guards - check guard for wildcard/ident patterns
                            if let Some(guard_expr) = &arm.guard {
                                self.generate_expr(func, guard_expr, scratch_local, locals)?;
                                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                                self.generate_expr(func, &arm.body, scratch_local, locals)?;
                                func.instruction(&Instruction::Drop);
                                func.instruction(&Instruction::Br(1));
                                func.instruction(&Instruction::End);
                            } else {
                                self.generate_expr(func, &arm.body, scratch_local, locals)?;
                                func.instruction(&Instruction::Drop);
                                func.instruction(&Instruction::Br(0));
                            }
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
            ast::Expr::Move { expr, .. } => {
                // Phase 4A: Move expression - just generate the inner expression
                // The ownership transfer is handled by the borrow checker
                self.generate_expr(func, expr, scratch_local, locals)?;
            },
            ast::Expr::Borrow { expr, .. } => {
                // Phase 4A: Borrow expression - generate the inner expression
                // References are compile-time only, no runtime representation needed
                self.generate_expr(func, expr, scratch_local, locals)?;
            },
            ast::Expr::Perform { args, .. } => {
                // Phase 4A: Perform effect - for now, just evaluate args
                // Full effect system would require runtime support
                for arg in args {
                    self.generate_expr(func, arg, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Handle { body, .. } => {
                // Phase 4A: Handle effects - execute body
                // Full effect handlers would require continuation support
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Resume { value, .. } => {
                // Phase 4A: Resume from handler - evaluate value
                // Full resume would require continuation support
                self.generate_expr(func, value, scratch_local, locals)?;
            },
            ast::Expr::InlineAsm { instructions, .. } => {
                // Phase 4A: Inline assembly - emit WASM instructions directly
                for instr in instructions {
                    self.emit_wasm_instruction(func, instr)?;
                }
            },
            ast::Expr::Placeholder(_) => {
                // Phase 4C: Placeholder should not appear in codegen
                // It should be converted to PartialApplication during parsing
                return Err("Unexpected placeholder in codegen".to_string());
            },
            ast::Expr::PartialApplication { func: partial_func, args, .. } => {
                // Phase 4C: Partial application creates a closure
                // For now, we'll generate a simple function pointer
                // In a full implementation, this would create a closure with captured arguments
                
                // Generate the original function
                self.generate_expr(func, partial_func, scratch_local, locals)?;
                
                // Store fixed arguments (simplified - would need proper closure support)
                for arg in args {
                    match arg {
                        ast::PartialArg::Fixed(expr) => {
                            self.generate_expr(func, expr, scratch_local, locals)?;
                        }
                        ast::PartialArg::Placeholder => {
                            // Placeholder - will be filled when the partial function is called
                        }
                    }
                }
            },
            ast::Expr::Lazy { expr, .. } => {
                // Phase 4C: Lazy evaluation - store expression for later evaluation
                // For now, just evaluate immediately (full lazy support would need thunks)
                self.generate_expr(func, expr, scratch_local, locals)?;
            },
            ast::Expr::Force { expr, .. } => {
                // Phase 4C: Force evaluation - evaluate the lazy expression
                self.generate_expr(func, expr, scratch_local, locals)?;
            },
            ast::Expr::Do { bindings, result, .. } => {
                // Phase 4C: Monadic do-notation - desugar to bind operations
                for binding in bindings {
                    self.generate_expr(func, &binding.expr, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                self.generate_expr(func, result, scratch_local, locals)?;
            },
            ast::Expr::Spawn { body, .. } => {
                // Phase 4D: Spawn - placeholder implementation
                // In a real implementation, this would create a new thread/task
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Channel { .. } => {
                // Phase 4D: Channel - placeholder (returns tuple of sender/receiver)
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Send { channel, value, .. } => {
                // Phase 4D: Send - placeholder
                self.generate_expr(func, channel, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                self.generate_expr(func, value, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Recv { channel, .. } => {
                // Phase 4D: Recv - placeholder
                self.generate_expr(func, channel, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Select { arms, .. } => {
                // Phase 4D: Select - placeholder (execute first arm)
                if let Some(arm) = arms.first() {
                    for stmt in &arm.body.stmts {
                        self.generate_stmt(func, stmt, scratch_local, locals)?;
                    }
                    if let Some(expr) = &arm.body.expr {
                        self.generate_expr(func, expr, scratch_local, locals)?;
                    } else {
                        func.instruction(&Instruction::I32Const(0));
                    }
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Scope { body, .. } => {
                // Phase 4D: Scope - execute body
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Atomic { target, value, operation, .. } => {
                // Phase 4D: Atomic operations - placeholder
                self.generate_expr(func, target, scratch_local, locals)?;
                if let Some(v) = value {
                    self.generate_expr(func, v, scratch_local, locals)?;
                    // Perform atomic operation based on operation type
                    match operation {
                        ast::AtomicOp::FetchAdd => {
                            func.instruction(&Instruction::I32Add);
                        },
                        ast::AtomicOp::FetchSub => {
                            func.instruction(&Instruction::I32Sub);
                        },
                        _ => {
                            func.instruction(&Instruction::Drop);
                        },
                    }
                }
            },
            ast::Expr::Lock { mutex, .. } => {
                // Phase 4D: Lock - placeholder
                self.generate_expr(func, mutex, scratch_local, locals)?;
            },
            ast::Expr::ReadLock { rwlock, .. } => {
                // Phase 4D: ReadLock - placeholder
                self.generate_expr(func, rwlock, scratch_local, locals)?;
            },
            ast::Expr::WriteLock { rwlock, .. } => {
                // Phase 4D: WriteLock - placeholder
                self.generate_expr(func, rwlock, scratch_local, locals)?;
            },
            ast::Expr::FutureJoin { futures, .. } => {
                // Phase 4D: Future::join - placeholder (execute all futures)
                for future in futures {
                    self.generate_expr(func, future, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::FutureSelect { futures, .. } => {
                // Phase 4D: Future::select - placeholder (execute first)
                if let Some(first) = futures.first() {
                    self.generate_expr(func, first, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::FutureRace { futures, .. } => {
                // Phase 4D: Future::race - placeholder (execute first)
                if let Some(first) = futures.first() {
                    self.generate_expr(func, first, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Timeout { duration, .. } => {
                // Phase 4D: Timeout - placeholder
                self.generate_expr(func, duration, scratch_local, locals)?;
            },
            ast::Expr::StreamFromIter { iter, .. } => {
                // Phase 4D: Stream::from_iter - placeholder
                self.generate_expr(func, iter, scratch_local, locals)?;
            },
            ast::Expr::StreamMap { stream, mapper, .. } => {
                // Phase 4D: Stream map - placeholder
                self.generate_expr(func, stream, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                self.generate_expr(func, mapper, scratch_local, locals)?;
            },
            ast::Expr::StreamFilter { stream, predicate, .. } => {
                // Phase 4D: Stream filter - placeholder
                self.generate_expr(func, stream, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                self.generate_expr(func, predicate, scratch_local, locals)?;
            },
            ast::Expr::StreamCollect { stream, .. } => {
                // Phase 4D: Stream collect - placeholder
                self.generate_expr(func, stream, scratch_local, locals)?;
            },
            ast::Expr::ParIter { collection, .. } => {
                // Phase 4D: par_iter - placeholder (returns parallel iterator)
                self.generate_expr(func, collection, scratch_local, locals)?;
            },
            ast::Expr::Try { body, catch_clauses: _, .. } => {
                // Phase 4E: Try-catch - execute body, catch clauses are for error handling
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
                // Catch clauses would be handled by runtime error handling
            },
            ast::Expr::TryOperator { expr, .. } => {
                // Phase 4E: ? operator - unwrap or early return
                // For now, just evaluate the expression
                self.generate_expr(func, expr, scratch_local, locals)?;
            },
            ast::Expr::Guard { condition, else_block, .. } => {
                // Phase 4E: Guard clause - if condition is false, execute else block
                self.generate_expr(func, condition, scratch_local, locals)?;
                // If false, execute else block (which should return/break)
                for stmt in &else_block.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &else_block.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                }
            },
            ast::Expr::LabeledBlock { block, .. } => {
                // Phase 4E: Labeled block - execute block
                for stmt in &block.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &block.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::BreakWithValue { value, .. } => {
                // Phase 4E: Break with value
                if let Some(v) = value {
                    self.generate_expr(func, v, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            },
            ast::Expr::Catch { expr, handler, .. } => {
                // Phase 4E: Catch expression - try expr, if error use handler
                self.generate_expr(func, expr, scratch_local, locals)?;
                // In a full implementation, would check for error and call handler
                // For now, just evaluate both
                func.instruction(&Instruction::Drop);
                self.generate_expr(func, handler, scratch_local, locals)?;
            },
            ast::Expr::Panic { message, .. } => {
                // Phase 4E: Panic - unreachable after this point
                if let Some(msg) = message {
                    self.generate_expr(func, msg, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Unreachable);
            },
            ast::Expr::CfgExpr { condition, then_expr, else_expr, .. } => {
                // Phase 4E: Conditional compilation - evaluate at compile time
                // For now, always use then branch (would check condition in real impl)
                let _ = condition; // Suppress warning
                self.generate_expr(func, then_expr, scratch_local, locals)?;
                if let Some(_else_e) = else_expr {
                    // In real implementation, would conditionally compile
                }
            },
            ast::Expr::ConstAssert { condition, message, .. } => {
                // Phase 4E: Const assertion - checked at compile time
                // For now, just evaluate condition
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
                if let Some(msg) = message {
                    self.generate_expr(func, msg, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Unreachable { message, .. } => {
                // Phase 4E: Unreachable - optimization hint
                if let Some(msg) = message {
                    self.generate_expr(func, msg, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Unreachable);
            },
            ast::Expr::MacroInvocation { args, .. } => {
                // Phase 4F: Macro invocation - expand at compile time
                // For now, just evaluate args
                for arg in args {
                    self.generate_expr(func, arg, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::TypeInfo { .. } => {
                // Phase 4F: Type reflection - compile-time only
                func.instruction(&Instruction::I32Const(0));
            },
            ast::Expr::Quote { .. } => {
                // Phase 4F: Quote - compile-time code generation
                func.instruction(&Instruction::I32Const(0));
            },
            // Phase 4G: String slicing
            ast::Expr::StringSlice { string, .. } => {
                // Generate string slice operation
                self.generate_expr(func, string, scratch_local, locals)?;
                // For now, just return the string as-is
                // Full implementation would handle range extraction
            },
            // Phase 4G: Format strings
            ast::Expr::FormatString { parts, .. } => {
                // Generate format string by concatenating parts
                if parts.is_empty() {
                    // Empty format string
                    let (ptr, len) = self.intern_string("");
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                } else {
                    // Generate first part
                    match &parts[0] {
                        ast::FormatPart::Text(text) => {
                            let (ptr, len) = self.intern_string(text);
                            func.instruction(&Instruction::I32Const(ptr as i32));
                            func.instruction(&Instruction::I32Const(len as i32));
                        },
                        ast::FormatPart::Formatted { expr, .. } => {
                            self.generate_expr(func, expr, scratch_local, locals)?;
                            // Convert to string if needed (simplified - assumes already string)
                        },
                    }
                    
                    // Concatenate remaining parts
                    for part in &parts[1..] {
                        match part {
                            ast::FormatPart::Text(text) => {
                                let (ptr, len) = self.intern_string(text);
                                func.instruction(&Instruction::I32Const(ptr as i32));
                                func.instruction(&Instruction::I32Const(len as i32));
                            },
                            ast::FormatPart::Formatted { expr, .. } => {
                                self.generate_expr(func, expr, scratch_local, locals)?;
                            },
                        }
                        // TODO: Call string concatenation function
                        // For now, just keep the last value on stack
                    }
                }
            },
            // Phase 4G: Destructuring assignment
            ast::Expr::DestructuringAssign { value, .. } => {
                // Generate value and then destructure
                self.generate_expr(func, value, scratch_local, locals)?;
                // Pattern matching would extract components
            },
            // Phase 4G: Range expression
            ast::Expr::Range { start, end, step, .. } => {
                // Generate range as a tuple (start, end, step)
                if let Some(s) = start {
                    self.generate_expr(func, s, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
                if let Some(e) = end {
                    self.generate_expr(func, e, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(-1)); // -1 means end
                }
                if let Some(st) = step {
                    self.generate_expr(func, st, scratch_local, locals)?;
                } else {
                    func.instruction(&Instruction::I32Const(1)); // default step
                }
            },
            ast::Expr::ListComprehension { .. } => {
                func.instruction(&Instruction::I32Const(0));
            },
        }
        Ok(())
    }

    /// Phase 4A: Emits a WebAssembly instruction from inline assembly.
    fn emit_wasm_instruction(&self, func: &mut Function, instr: &ast::WasmInstruction) -> Result<(), String> {
        use wasm_encoder::Instruction;
        
        match instr {
            ast::WasmInstruction::LocalGet(idx) => {
                func.instruction(&Instruction::LocalGet(*idx));
            }
            ast::WasmInstruction::LocalSet(idx) => {
                func.instruction(&Instruction::LocalSet(*idx));
            }
            ast::WasmInstruction::LocalTee(idx) => {
                func.instruction(&Instruction::LocalTee(*idx));
            }
            ast::WasmInstruction::Drop => {
                func.instruction(&Instruction::Drop);
            }
            ast::WasmInstruction::Select => {
                func.instruction(&Instruction::Select);
            }
            ast::WasmInstruction::I32Const(val) => {
                func.instruction(&Instruction::I32Const(*val));
            }
            ast::WasmInstruction::I64Const(val) => {
                func.instruction(&Instruction::I64Const(*val));
            }
            ast::WasmInstruction::F32Const(val) => {
                func.instruction(&Instruction::F32Const((*val).into()));
            }
            ast::WasmInstruction::F64Const(val) => {
                func.instruction(&Instruction::F64Const((*val).into()));
            }
            ast::WasmInstruction::I32Add => {
                func.instruction(&Instruction::I32Add);
            }
            ast::WasmInstruction::I32Sub => {
                func.instruction(&Instruction::I32Sub);
            }
            ast::WasmInstruction::I32Mul => {
                func.instruction(&Instruction::I32Mul);
            }
            ast::WasmInstruction::I32DivS => {
                func.instruction(&Instruction::I32DivS);
            }
            ast::WasmInstruction::I32DivU => {
                func.instruction(&Instruction::I32DivU);
            }
            ast::WasmInstruction::F64Add => {
                func.instruction(&Instruction::F64Add);
            }
            ast::WasmInstruction::F64Sub => {
                func.instruction(&Instruction::F64Sub);
            }
            ast::WasmInstruction::F64Mul => {
                func.instruction(&Instruction::F64Mul);
            }
            ast::WasmInstruction::F64Div => {
                func.instruction(&Instruction::F64Div);
            }
            ast::WasmInstruction::I32Eq => {
                func.instruction(&Instruction::I32Eq);
            }
            ast::WasmInstruction::I32Ne => {
                func.instruction(&Instruction::I32Ne);
            }
            ast::WasmInstruction::I32LtS => {
                func.instruction(&Instruction::I32LtS);
            }
            ast::WasmInstruction::I32GtS => {
                func.instruction(&Instruction::I32GtS);
            }
            ast::WasmInstruction::F64Eq => {
                func.instruction(&Instruction::F64Eq);
            }
            ast::WasmInstruction::F64Ne => {
                func.instruction(&Instruction::F64Ne);
            }
            ast::WasmInstruction::F64Lt => {
                func.instruction(&Instruction::F64Lt);
            }
            ast::WasmInstruction::F64Gt => {
                func.instruction(&Instruction::F64Gt);
            }
            ast::WasmInstruction::I32Load => {
                func.instruction(&Instruction::I32Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
            }
            ast::WasmInstruction::I32Store => {
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
            }
            ast::WasmInstruction::F64Load => {
                func.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            ast::WasmInstruction::F64Store => {
                func.instruction(&Instruction::F64Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            ast::WasmInstruction::Call(idx) => {
                func.instruction(&Instruction::Call(*idx));
            }
            ast::WasmInstruction::Return => {
                func.instruction(&Instruction::Return);
            }
            ast::WasmInstruction::Raw(_) => {
                // Raw instructions are not directly supported - would need custom parsing
                // For now, emit a nop
                func.instruction(&Instruction::Nop);
            }
        }
        
        Ok(())
    }

    /// Analyses a block to determine what local variables are needed.
    ///
    /// Returns a map from variable names to local indices, and a list of
    /// WASM value types for each local. The first local is always a scratch
    /// variable used for temporary storage.
    #[allow(dead_code)]
    fn analyze_locals(&self, block: &ast::Block) -> (HashMap<String, u32>, Vec<ValType>) {
        let mut locals_map = HashMap::new();
        // Reserve local 0 as a scratch variable for intermediate values.
        let mut local_types = vec![ValType::I32];
        self.collect_locals(block, &mut locals_map, &mut local_types);
        (locals_map, local_types)
    }
    
    /// Phase 4G: Collect locals from a pattern (handles tuple and record destructuring)
    fn collect_pattern_locals(
        &self,
        pattern: &ast::Pattern,
        type_ann: &Option<Type>,
        value: &Option<ast::Expr>,
        param_count: u32,
        locals_map: &mut HashMap<String, u32>,
        local_types: &mut Vec<ValType>,
    ) {
        match pattern {
            ast::Pattern::Ident(id) => {
                let idx = param_count + local_types.len() as u32;
                locals_map.insert(id.name.to_string(), idx);
                
                // Infer type from value if no type annotation
                let val_type = if let Some(ty) = type_ann {
                    self.map_type(Some(ty))
                } else if let Some(val_expr) = value {
                    // Simple type inference based on expression type
                    match val_expr {
                        ast::Expr::StructInit { .. } => ValType::I32,
                        ast::Expr::Call { func: callee, .. } => {
                            // If calling a function, check its return type
                            if let ast::Expr::Ident(fn_id) = callee.as_ref() {
                                self.func_return_types.get(fn_id.name.as_ref())
                                    .copied()
                                    .unwrap_or(ValType::F64)
                            } else {
                                self.map_type(None)
                            }
                        },
                        ast::Expr::Binary { .. } => {
                            // Binary operations might return structs (operator overloading)
                            ValType::I32
                        },
                        _ => self.map_type(None)
                    }
                } else {
                    self.map_type(None)
                };
                
                local_types.push(val_type);
            }
            ast::Pattern::Tuple(patterns) => {
                // For tuple destructuring, each element gets its own local
                for sub_pattern in patterns {
                    self.collect_pattern_locals(sub_pattern, &None, &None, param_count, locals_map, local_types);
                }
            }
            ast::Pattern::Record(fields) => {
                // For record destructuring, each field gets its own local
                for (_field_name, field_pattern) in fields {
                    self.collect_pattern_locals(field_pattern, &None, &None, param_count, locals_map, local_types);
                }
            }
            _ => {
                // Other patterns (wildcard, literal) don't create locals
            }
        }
    }
    
    // Phase 4G: Analyze locals including function parameters
    fn analyze_locals_with_params(&self, params: &[(ast::Pattern, Option<Type>, Option<ast::Expr>)], block: &ast::Block) -> (HashMap<String, u32>, Vec<ValType>) {
        let mut locals_map = HashMap::new();
        
        // Add function parameters first (they are WASM function parameters, indices 0, 1, 2, ...)
        for (idx, (pattern, _ty, _default)) in params.iter().enumerate() {
            match pattern {
                ast::Pattern::Ident(id) => {
                    locals_map.insert(id.name.to_string(), idx as u32);
                }
                _ => {
                    // Handle other pattern types (tuple destructuring, etc.)
                }
            }
        }
        
        // Local variables start after parameters
        // The scratch local is at index params.len()
        // The struct_ptr local is at index params.len() + 1
        // User locals start at params.len() + 2
        let param_count = params.len() as u32;
        let mut local_types = vec![ValType::I32, ValType::I32]; // Scratch variable and struct pointer
        
        // Collect local variables from the block
        for stmt in &block.stmts {
            if let ast::Stmt::Let { pattern, type_ann, value, .. } = stmt {
                // Phase 4G: Handle different pattern types
                self.collect_pattern_locals(pattern, type_ann, value, param_count, &mut locals_map, &mut local_types);
            }
        }
        
        (locals_map, local_types)
    }

    /// Recursively collects local variable declarations from a block.
    #[allow(dead_code)]
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
            Some(Type::ConstArray { .. }) => ValType::I32,  // Phase 4A: Const array pointer
            Some(Type::Tuple(_)) => ValType::I32,  // Pointer to tuple.
            Some(Type::Record(_)) => ValType::I32,  // Pointer to record.
            Some(Type::Generic(_)) => ValType::I32,  // Pointer to struct instance.
            Some(Type::Function { .. }) => ValType::I32,  // Function reference.
            Some(Type::Reference { inner, .. }) => {
                // Phase 4A: References have same representation as their inner type
                // (they're just pointers/values with borrow checking at compile time)
                self.map_type(Some(inner))
            }
            Some(Type::Union(types)) => {
                // Phase 4B: Union types - use first type's representation
                // (runtime type checking would be needed for full support)
                if let Some(first) = types.first() {
                    self.map_type(Some(first))
                } else {
                    ValType::I32
                }
            }
            Some(Type::Intersection(types)) => {
                // Phase 4B: Intersection types - use first type's representation
                if let Some(first) = types.first() {
                    self.map_type(Some(first))
                } else {
                    ValType::I32
                }
            }
            Some(Type::HigherKinded { .. }) => {
                // Phase 4B: Higher-kinded types are type constructors
                // They don't have a runtime representation themselves
                // This should only appear in type signatures, not runtime values
                ValType::I32  // Placeholder
            }
            Some(Type::AppliedHigherKinded { args, .. }) => {
                // Phase 4B: Applied higher-kinded types use the first arg's representation
                // Example: Option<i32> uses i32's representation
                if let Some(first) = args.first() {
                    self.map_type(Some(first))
                } else {
                    ValType::I32
                }
            }
            Some(Type::PhantomData(_)) => {
                // Phase 4B: PhantomData is zero-sized, no runtime representation
                // It exists only for compile-time type checking
                // We use i32 as a placeholder, but it should never be instantiated
                ValType::I32
            }
            Some(Type::Refinement { base, .. }) => {
                // Phase 4B: Refinement types use the base type's representation
                // The predicate is enforced at compile-time or with runtime checks
                self.map_type(Some(base))
            }
            Some(Type::Dependent { .. }) => {
                // Phase 4B: Dependent types have runtime value parameters
                // For now, use i32 as the representation (could be more sophisticated)
                ValType::I32
            }
            Some(Type::TypeLevelApp { .. }) | Some(Type::TypeLevelLit(_)) => {
                // Phase 4B: Type-level computations are compile-time only
                // They don't have runtime representation
                ValType::I32
            }
            Some(Type::Existential { .. }) => {
                // Phase 4B: Existential types hide concrete type
                // Use i32 as generic representation (could be trait object pointer)
                ValType::I32
            }
            Some(Type::GADTReturn { .. }) => {
                // Phase 4B: GADT return types are enum variants
                // Use i32 as pointer to tagged union
                ValType::I32
            }
            Some(Type::ImmutableVec(_)) | Some(Type::ImmutableSet(_)) => {
                // Phase 4C: Immutable collections are pointers to persistent data structures
                ValType::I32
            }
            Some(Type::ImmutableMap { .. }) => {
                // Phase 4C: Immutable map is pointer to persistent data structure
                ValType::I32
            }
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

    /// Phase 4B: Process GADT enum definition
    /// 
    /// Enums are represented as tagged unions in memory:
    /// - First 4 bytes: discriminant (variant tag)
    /// - Remaining bytes: largest variant's data
    fn process_enum(&mut self, e: &ast::Enum) -> Result<(), String> {
        let mut max_variant_size = 0u32;
        let mut variant_info = HashMap::new();
        
        for (idx, variant) in e.variants.iter().enumerate() {
            let mut variant_size = 0u32;
            let mut field_offsets = HashMap::new();
            
            match &variant.fields {
                ast::VariantFields::Unit => {
                    // No data, just the discriminant
                }
                ast::VariantFields::Tuple(types) => {
                    let mut offset = 0u32;
                    for (field_idx, ty) in types.iter().enumerate() {
                        let val_type = self.map_type(Some(ty));
                        let align = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
                        while offset % align != 0 { offset += 1; }
                        field_offsets.insert(field_idx.to_string(), (offset, val_type));
                        let size = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
                        offset += size;
                    }
                    variant_size = offset;
                }
                ast::VariantFields::Named(fields) => {
                    let mut offset = 0u32;
                    for (name, ty) in fields {
                        let val_type = self.map_type(Some(ty));
                        let align = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
                        while offset % align != 0 { offset += 1; }
                        field_offsets.insert(name.name.to_string(), (offset, val_type));
                        let size = match val_type { ValType::F64 => 8, ValType::I32 => 4, _ => 8 };
                        offset += size;
                    }
                    variant_size = offset;
                }
            }
            
            if variant_size > max_variant_size {
                max_variant_size = variant_size;
            }
            
            variant_info.insert(variant.name.name.to_string(), (idx as u32, field_offsets));
        }
        
        // Total enum size: 4 bytes (discriminant) + max variant size
        let total_size = 4 + max_variant_size;
        
        // Store enum layout info (could be used for pattern matching)
        // For now, we just track the size
        self.struct_map.insert(e.name.name.to_string(), (total_size, HashMap::new()));
        
        Ok(())
    }
    
    // Phase 4G: Convert Type to String for trait impl lookup
    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Generic(ident) => ident.name.to_string(),
            _ => format!("{:?}", ty),
        }
    }
    
    // Phase 4G: Get trait method for operator overloading
    fn get_trait_method_for_op(&self, left_expr: &ast::Expr, op: &ast::BinaryOp) -> Result<Option<(String, String)>, String> {
        // Map operator to trait name
        let trait_name = match op {
            ast::BinaryOp::Add => "Add",
            ast::BinaryOp::Sub => "Sub",
            ast::BinaryOp::Mul => "Mul",
            ast::BinaryOp::Div => "Div",
            _ => return Ok(None), // No trait for this operator
        };
        
        // Get the type of the left operand
        // Try expr_types first, then param_types if it's an identifier
        let left_type_opt = self.expr_types.get(&left_expr.span()).or_else(|| {
            if let ast::Expr::Ident(id) = left_expr {
                self.param_types.get(id.name.as_ref())
            } else {
                None
            }
        });
        
        let left_type = if let Some(ty) = left_type_opt {
            self.type_to_string(ty)
        } else {
            return Ok(None); // Can't determine type
        };
        
        // Check if there's a trait implementation
        if let Some(trait_impl) = self.trait_impls.get(&(trait_name.to_string(), left_type.clone())) {
            // Find the method name (typically lowercase of trait name)
            let method_name = match trait_name {
                "Add" => "add",
                "Sub" => "sub",
                "Mul" => "mul",
                "Div" => "div",
                _ => return Ok(None),
            };
            
            // Check if the method exists in the trait impl
            for method in &trait_impl.methods {
                if method.name.name.as_ref() == method_name {
                    return Ok(Some((method.name.name.to_string(), left_type)));
                }
            }
        }
        
        Ok(None)
    }
}
