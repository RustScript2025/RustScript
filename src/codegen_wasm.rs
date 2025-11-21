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
        }
    }

    pub fn generate(mut self, ast_module: &ast::Module) -> Result<Vec<u8>, String> {
        // 0. Setup standard types
        // Type 0: (i32, i32) -> ()  // For console.log(str_ptr, str_len)
        self.types.function(vec![ValType::I32, ValType::I32], vec![]);
        
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
                _ => {
                    // Structs and other items not yet supported in codegen
                }
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
        let func_idx = self.funcs.len() - 1 + self.imports.len(); // Adjust for imports
        
        self.func_map.insert(func.name.name.to_string(), func_idx);

        // Export if needed
        if func.name.name.as_ref() == "main" {
            self.exports.export(func.name.name.as_ref(), ExportKind::Func, func_idx);
        }

        // Generate function body (CodeSection)
        let mut wasm_func = Function::new_with_locals_types(vec![]);
        
        for stmt in &func.body.stmts {
            self.generate_stmt(&mut wasm_func, stmt)?;
        }
        
        // Implicit return if needed
        wasm_func.instruction(&Instruction::End);
        self.codes.function(&wasm_func);
        Ok(())
    }

    fn generate_stmt(&mut self, func: &mut Function, stmt: &ast::Stmt) -> Result<(), String> {
        match stmt {
            ast::Stmt::Expr(expr, _) => {
                self.generate_expr(func, expr)?;
                // Drop result if needed (simplification: assume void for now)
            },
            ast::Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.generate_expr(func, e)?;
                }
                func.instruction(&Instruction::Return);
            },
            ast::Stmt::Let { value, .. } => {
                // For now, just evaluate the value to ensure side effects run
                if let Some(expr) = value {
                    self.generate_expr(func, expr)?;
                    func.instruction(&Instruction::Drop);
                }
            },
            _ => {
                // Other statements not yet implemented
            }
        }
        Ok(())
    }

    fn generate_expr(&mut self, func: &mut Function, expr: &ast::Expr) -> Result<(), String> {
        match expr {
            ast::Expr::Literal(lit, _) => match lit {
                ast::Literal::Number(n) => {
                    func.instruction(&Instruction::F64Const(*n));
                },
                ast::Literal::Boolean(b) => {
                    func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                },
                ast::Literal::String(s) => {
                    let (offset, len) = self.intern_string(s);
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                },
                _ => {} 
            },
            ast::Expr::Call { func: callee, args, .. } => {
                // Handle console.log special case or general calls
                if let ast::Expr::Ident(id) = callee.as_ref() {
                    if id.name.as_ref() == "console.log" || id.name.as_ref() == "console.error" {
                         // Push args (expecting string)
                         for arg in args {
                             self.generate_expr(func, arg)?;
                         }
                         // Call import
                         if let Some(idx) = self.import_map.get(id.name.as_ref()) {
                             func.instruction(&Instruction::Call(*idx));
                         }
                    }
                }
            },
            ast::Expr::Binary { left, op, right, .. } => {
                self.generate_expr(func, left)?;
                self.generate_expr(func, right)?;
                match op {
                    ast::BinaryOp::Add => { func.instruction(&Instruction::F64Add); },
                    ast::BinaryOp::Sub => { func.instruction(&Instruction::F64Sub); },
                    ast::BinaryOp::Mul => { func.instruction(&Instruction::F64Mul); },
                    ast::BinaryOp::Div => { func.instruction(&Instruction::F64Div); },
                    _ => {} 
                }
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

    fn map_type(&self, ty: Option<&Type>) -> ValType {
        match ty {
            Some(Type::Number) => ValType::F64,
            Some(Type::Boolean) => ValType::I32,
            _ => ValType::F64, 
        }
    }
}
