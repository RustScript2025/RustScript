use wasm_bindgen::prelude::*;
use std::panic;

pub mod ast;
pub mod lexer;
pub mod typechecker;
pub mod compiler;
pub mod diagnostics;
pub mod sourcemap;
pub mod codegen_wasm;
pub mod std_lib;
pub mod borrow_checker;
pub mod memory;

// Generate the parser module using lalrpop
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

/// Initialize the panic hook for better error messages in the browser console.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    Ok(())
}

/// Compiles RustScript source code to WebAssembly binary.
#[wasm_bindgen]
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, JsValue> {
    // 1. Parse
    // Note: We use the generated parser. 
    // The parser expects a generic error type, so we map errors to strings for JS.
    let parser = parser::ProgramParser::new();
    let ast = parser.parse(source)
        .map_err(|e| JsValue::from_str(&format!("Parse Error: {:?}", e)))?;
        
    // 2. Safety Checks (Borrow Checker)
    let mut borrow_checker = borrow_checker::BorrowChecker::new();
    if let Err(errors) = borrow_checker.check_module(&ast) {
        let error_msg = errors.join("\n");
        return Err(JsValue::from_str(&format!("Borrow Check Error:\n{}", error_msg)));
    }
        
    // 3. Code Generation (WASM)
    let generator = codegen_wasm::WasmGenerator::new();
    let wasm_bytes = generator.generate(&ast)
        .map_err(|e| JsValue::from_str(&format!("Codegen Error: {}", e)))?;
    
    Ok(wasm_bytes)
}

/// Compiles and executes RustScript source code immediately.
#[wasm_bindgen]
pub async fn run_script(source: &str) -> Result<(), JsValue> {
    let wasm_bytes = compile_to_wasm(source)?;
    
    // Instantiate the WASM module
    // We need to provide imports if the generated WASM expects them (e.g. console.log)
    // For now, our codegen doesn't generate imports, but we'll likely need them soon.
    // Create imports for the WASM module
    let imports = js_sys::Object::new();
    let console_obj = js_sys::Object::new();
    
    // Helper to create a simple logging function
    // Note: Real string decoding requires access to the instance's memory, 
    // which is tricky to wire up here without a JS shim. 
    // For now, we just log that it was called.
    let log_fn = js_sys::Function::new_with_args_and_body(
        "ptr, len",
        "console.log('RustScript log (ptr, len):', ptr, len);"
    );
    let error_fn = js_sys::Function::new_with_args_and_body(
        "ptr, len",
        "console.error('RustScript error (ptr, len):', ptr, len);"
    );
    
    js_sys::Reflect::set(&console_obj, &"log".into(), &log_fn)?;
    js_sys::Reflect::set(&console_obj, &"error".into(), &error_fn)?;
    js_sys::Reflect::set(&imports, &"console".into(), &console_obj)?;
    
    let module = wasm_bindgen_futures::JsFuture::from(
        WebAssembly::instantiate_buffer(&wasm_bytes, &imports)
    ).await?;
    
    let instance = js_sys::Reflect::get(&module, &"instance".into())?;
    let exports = js_sys::Reflect::get(&instance, &"exports".into())?;
    
    // Find and run 'main'
    let main_fn = js_sys::Reflect::get(&exports, &"main".into())?;
    
    if main_fn.is_function() {
        let func = js_sys::Function::from(main_fn);
        func.call0(&JsValue::NULL)?;
    } else {
        log("No 'main' function found in script.");
    }
    
    Ok(())
}
