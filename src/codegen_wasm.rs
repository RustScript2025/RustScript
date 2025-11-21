use wasm_encoder::{
    CodeSection, DataSection, ExportKind, ExportSection, Function, FunctionSection, 
    Instruction, Module, TypeSection, ValType, ImportSection, EntityType, MemoryType,
    MemorySection, ConstExpr
};
use crate::ast::{self, Type};
use std::collections::HashMap;

pub struct WasmGenerator {
    module: Module,
    types: TypeSection,
    imports: ImportSection,
    funcs: FunctionSection,
    exports: ExportSection,
    codes: CodeSection,
    data: DataSection,
    memory: MemorySection,
    
    // Track string literals -> (offset, length)
    string_literals: HashMap<String, (u32, u32)>,
    next_data_offset: u32,
    
    // Map function names to type index and func index
    func_map: HashMap<String, u32>,
    import_map: HashMap<String, u32>,
    
    // Map struct name -> (size, field_offsets: HashMap<String, (u32, ValType)>)
    struct_map: HashMap<String, (u32, HashMap<String, (u32, ValType)>)>,
    
    // Type info from typechecker
    expr_types: HashMap<ast::Span, Type>,
}

impl WasmGenerator {
    pub fn new() -> Self {
        let mut imports = ImportSection::new();
        let mut import_map = HashMap::new();
        
        // Import console.log as index 0
        // (param i32, i32) -> [] (ptr, len)
        imports.import("console", "log", EntityType::Function(0)); 
        import_map.insert("console.log".to_string(), 0);
        
        imports.import("console", "error", EntityType::Function(0));
        import_map.insert("console.error".to_string(), 1);

        // Import malloc as index 2
        // (param i32) -> (i32)
        imports.import("env", "malloc", EntityType::Function(1)); // Type 1: (i32) -> (i32)
        import_map.insert("malloc".to_string(), 2);

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
            struct_map: HashMap::new(),
            expr_types: HashMap::new(),
        }
    }

    pub fn generate(mut self, ast_module: &ast::Module, expr_types: HashMap<ast::Span, Type>) -> Result<Vec<u8>, String> {
        self.expr_types = expr_types;
        
        // 0. Setup standard types
        // Type 0: (i32, i32) -> ()  // For console.log(str_ptr, str_len)
        self.types.function(vec![ValType::I32, ValType::I32], vec![]);
        // Type 1: (i32) -> (i32)    // For malloc(size) -> ptr
        self.types.function(vec![ValType::I32], vec![ValType::I32]);
        
        // Setup memory (1 page)
        self.memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
        });
        self.exports.export("memory", ExportKind::Memory, 0);

        // 1. Process all items
        for item in &ast_module.items {
            match item {
                ast::Item::Function(f) => self.process_function(f)?,
                ast::Item::Struct(s) => self.process_struct(s)?,
                _ => return Err(format!("Unsupported item type: {:?}", item)),
            }
        }

        // 2. Assemble the module
        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.funcs);
        self.module.section(&self.memory);
        self.module.section(&self.exports);
        self.module.section(&self.codes);
        self.module.section(&self.data);

        Ok(self.module.finish())
    }

    fn process_function(&mut self, func: &ast::Function) -> Result<(), String> {
        // Define function type signature
        let params: Vec<ValType> = func.params.iter()
            .map(|(_, ty)| self.map_type(ty.as_ref()))
            .collect();
            
        let results: Vec<ValType> = func.return_type.as_ref()
            .map(|ty| vec![self.map_type(ty)])
            .unwrap_or_default();

        // Add type to TypeSection
        self.types.function(params.clone(), results.clone());
        let type_idx = self.types.len() - 1;

        // Add function to FunctionSection
        self.funcs.function(type_idx);
                             self.generate_expr(func, arg, scratch_local, locals)?;
                         }
                         // Call import
                         if let Some(idx) = self.import_map.get(id.name.as_ref()) {
                             func.instruction(&Instruction::Call(*idx));
                         }
                    }
                }
            },
            ast::Expr::Binary { left, op, right, span: _ } => {
                self.generate_expr(func, left, scratch_local, locals)?;
                self.generate_expr(func, right, scratch_local, locals)?;
                match op {
                    ast::BinaryOp::Add => { func.instruction(&Instruction::F64Add); },
                    ast::BinaryOp::Sub => { func.instruction(&Instruction::F64Sub); },
                    ast::BinaryOp::Mul => { func.instruction(&Instruction::F64Mul); },
                    ast::BinaryOp::Div => { func.instruction(&Instruction::F64Div); },
                    ast::BinaryOp::Eq => { func.instruction(&Instruction::F64Eq); },
                    ast::BinaryOp::Lt => { func.instruction(&Instruction::F64Lt); },
                    ast::BinaryOp::Gt => { func.instruction(&Instruction::F64Gt); },
                    ast::BinaryOp::And => { func.instruction(&Instruction::I32And); },
                    ast::BinaryOp::Or => { func.instruction(&Instruction::I32Or); },
                    _ => {} 
                }
            },
            ast::Expr::If { condition, then_branch, else_branch, span: _ } => {
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                for stmt in &then_branch.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &then_branch.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop); // Block returns void for now
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
            ast::Expr::Loop { body, span: _ } => {
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Br(0)); // Infinite loop
                func.instruction(&Instruction::End);
            },
            ast::Expr::While { condition, body, span: _ } => {
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                
                self.generate_expr(func, condition, scratch_local, locals)?;
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::BrIf(1)); // Break if false
                
                for stmt in &body.stmts {
                    self.generate_stmt(func, stmt, scratch_local, locals)?;
                }
                if let Some(expr) = &body.expr {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                    func.instruction(&Instruction::Drop);
                }
                
                func.instruction(&Instruction::Br(0)); // Loop
                func.instruction(&Instruction::End); // End Loop
                func.instruction(&Instruction::End); // End Block
            },
            ast::Expr::StructInit { name, fields, span: _ } => {
                let (size, offsets) = self.struct_map.get(name.name.as_ref())
                    .ok_or_else(|| format!("Unknown struct: {}", name.name))?;
                
                // Call malloc
                func.instruction(&Instruction::I32Const(*size as i32));
                func.instruction(&Instruction::Call(2)); // malloc
                
                // Store ptr in scratch local
                func.instruction(&Instruction::LocalSet(scratch_local));
                
                // Initialize fields
                for (field_name, field_expr) in fields {
                    let (offset, val_type) = offsets.get(field_name.name.as_ref())
                        .ok_or_else(|| format!("Unknown field: {}", field_name.name))?;
                    
                    func.instruction(&Instruction::LocalGet(scratch_local));
                    func.instruction(&Instruction::I32Const(*offset as i32));
                    func.instruction(&Instruction::I32Add);
                    
                    self.generate_expr(func, field_expr, scratch_local, locals)?;
                    
                    match val_type {
                        ValType::I32 => func.instruction(&Instruction::I32Store(wasm_encoder::MemArg { offset: 0, align: 2, memory_index: 0 })),
                        ValType::F64 => func.instruction(&Instruction::F64Store(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 })),
                        _ => return Err("Unsupported field type".to_string()),
                    };
                }
                
                // Return ptr
                func.instruction(&Instruction::LocalGet(scratch_local));
            },
            ast::Expr::FieldAccess { expr, field, span: _ } => {
                 self.generate_expr(func, expr, scratch_local, locals)?;
                 
                 // Get struct type from expr
                 let expr_type = self.expr_types.get(expr.span())
                     .ok_or_else(|| "Missing type info for field access".to_string())?;
                 
                 let struct_name = match expr_type {
                     Type::Generic(id) => &id.name,
                     _ => return Err("Field access on non-struct type".to_string()),
                 };
                 
                 let (_, offsets) = self.struct_map.get(struct_name.as_ref())
                     .ok_or_else(|| format!("Unknown struct: {}", struct_name))?;
                 
                 let (offset, val_type) = offsets.get(field.name.as_ref())
                     .ok_or_else(|| format!("Unknown field: {}", field.name))?;
                 
                 func.instruction(&Instruction::I32Const(*offset as i32));
                 func.instruction(&Instruction::I32Add);
                 
                 match val_type {
                     ValType::I32 => func.instruction(&Instruction::I32Load(wasm_encoder::MemArg { offset: 0, align: 2, memory_index: 0 })),
                     ValType::F64 => func.instruction(&Instruction::F64Load(wasm_encoder::MemArg { offset: 0, align: 3, memory_index: 0 })),
                     _ => return Err("Unsupported field type".to_string()),
                 };
            },
            ast::Expr::Match { expr: value, arms, span: _ } => {
                 // Evaluate match value -> [val]
                 self.generate_expr(func, value, scratch_local, locals)?;
                 // Store in scratch to compare against arms
                 func.instruction(&Instruction::LocalSet(scratch_local));
                 
                 // We need a block to break out of when a match is found
                 func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                 
                 for arm in arms {
                     // For each arm, check if value matches pattern
                     match &arm.pattern {
                         ast::Pattern::Literal(lit, _) => {
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
                                     // Skip unsupported patterns for now
                                     func.instruction(&Instruction::Drop);
                                     func.instruction(&Instruction::I32Const(0)); // False
                                 }
                             }
                             
                             func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                             self.generate_expr(func, &arm.body, scratch_local, locals)?;
                             func.instruction(&Instruction::Drop); 
                             
                             func.instruction(&Instruction::Br(1)); // Break out of main match block
                             func.instruction(&Instruction::End); // End If
                         },
                         ast::Pattern::Wildcard(_) => {
                             // Wildcard matches everything
                             self.generate_expr(func, &arm.body, scratch_local, locals)?;
                             func.instruction(&Instruction::Drop);
                             func.instruction(&Instruction::Br(0)); // Break out of main match block
                         },
                         ast::Pattern::Ident(id) => {
                             // Identifier pattern matches everything AND binds the value
                             // For this MVP, we treat it as a wildcard match but we should ideally bind it.
                             // TODO: Implement true binding by allocating a local.
                             self.generate_expr(func, &arm.body, scratch_local, locals)?;
                             func.instruction(&Instruction::Drop);
                             func.instruction(&Instruction::Br(0));
                         },
                         _ => {} // Unsupported pattern
                     }
                 }
                 
                 func.instruction(&Instruction::End); // End Block
            },
            _ => {} 
        }
        Ok(())
    }

    fn intern_string(&mut self, s: &str) -> (u32, u32) {
        if let Some(&loc) = self.string_literals.get(s) {
            return loc;
        }
        
        let offset = self.next_data_offset;
        let bytes = s.as_bytes();
        let len = bytes.len() as u32;
        
        self.data.segment(
            0, // memory index
            ConstExpr::i32_const(offset as i32),
            bytes.iter().cloned()
        );
        
        self.next_data_offset += len;
        self.string_literals.insert(s.to_string(), (offset, len));
        
        (offset, len)
    }

    fn process_struct(&mut self, s: &ast::Struct) -> Result<(), String> {
        let mut field_offsets = HashMap::new();
        let mut current_offset = 0;
        
        for (name, ty) in &s.fields {
            let val_type = self.map_type(Some(ty));
            // Align to 8 bytes for F64, 4 for I32
            let align = match val_type {
                ValType::F64 => 8,
                ValType::I32 => 4,
                _ => 8,
            };
            
            // Padding
            while current_offset % align != 0 {
                current_offset += 1;
            }
            
            field_offsets.insert(name.name.to_string(), (current_offset, val_type));
            
            let size = match val_type {
                ValType::F64 => 8,
                ValType::I32 => 4,
                _ => 8,
            };
            current_offset += size;
        }
        
        self.struct_map.insert(s.name.name.to_string(), (current_offset, field_offsets));
        Ok(())
    }

    fn map_type(&self, ty: Option<&Type>) -> ValType {
        match ty {
            Some(Type::Number) => ValType::F64,
            Some(Type::Boolean) => ValType::I32,
            Some(Type::String) => ValType::I32, // Pointer
            Some(Type::Generic(_)) => ValType::I32, // Pointer to struct
            _ => ValType::F64, 
        }
    }

    fn analyze_locals(&self, block: &ast::Block) -> (HashMap<String, u32>, Vec<ValType>) {
        let mut locals_map = HashMap::new();
        let mut local_types = Vec::new();
        let mut next_index = 0; // Relative to user locals start
        
        let mut worklist = vec![block];
        while let Some(b) = worklist.pop() {
            for stmt in &b.stmts {
                if let ast::Stmt::Let { pattern, value, span: _, type_ann: _, mutable: _ } = stmt {
                    if let ast::Pattern::Ident(id) = pattern {
                        if !locals_map.contains_key(&id.name.to_string()) {
                            locals_map.insert(id.name.to_string(), next_index);
                            local_types.push(ValType::F64); // Default to F64
                            next_index += 1;
                        }
                    }
                }
            }
        }
        (locals_map, local_types)
    }

    fn generate_stmt(&mut self, func: &mut Function, stmt: &ast::Stmt, scratch_local: u32, locals: &HashMap<String, u32>) -> Result<(), String> {
        match stmt {
            ast::Stmt::Expr(expr, _) => {
                self.generate_expr(func, expr, scratch_local, locals)?;
                func.instruction(&Instruction::Drop);
            },
            ast::Stmt::Let { pattern, value, span: _, type_ann: _, mutable: _ } => {
                if let Some(expr) = value {
                    self.generate_expr(func, expr, scratch_local, locals)?;
                    if let ast::Pattern::Ident(id) = pattern {
                         if let Some(&idx) = locals.get(id.name.as_ref()) {
                             let base_offset = scratch_local + 1;
                             func.instruction(&Instruction::LocalSet(base_offset + idx));
                         }
                    } else {
                        func.instruction(&Instruction::Drop);
                    }
                }
            },
            ast::Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.generate_expr(func, e, scratch_local, locals)?;
                }
                func.instruction(&Instruction::Return);
            },
            _ => {}
        }
        Ok(())
    }
}
