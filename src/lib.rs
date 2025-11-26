//! RustScript compiler library.
//! 
//! Author: Michael Lauzon

use wasm_bindgen::prelude::*;
use std::panic;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod compiler;
pub mod diagnostics;
pub mod sourcemap;
pub mod codegen_wasm;
pub mod std_lib;
pub mod borrow_checker;
pub mod memory;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    Ok(())
}

#[wasm_bindgen]
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, JsValue> {
    let ast = parser::parse_program(source)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {e}")))?;
        
    let mut borrow_checker = borrow_checker::BorrowChecker::new();
    if let Err(errors) = borrow_checker.check_module(&ast) {
        let error_msg = errors.join("\n");
        return Err(JsValue::from_str(&format!("Borrow check failed:\n{error_msg}")));
    }
    
    let mut type_checker = typechecker::TypeChecker::new();
    let expr_types = type_checker.check(&ast)
        .map_err(|e| JsValue::from_str(&format!("Type error: {e}")))?;
    
    let generator = codegen_wasm::WasmGenerator::new();
    let wasm_bytes = generator.generate(&ast, expr_types)
        .map_err(|e| JsValue::from_str(&format!("Code generation error: {e}")))?;
    
    Ok(wasm_bytes)
}

#[wasm_bindgen]
pub async fn run_script(source: &str) -> Result<(), JsValue> {
    let wasm_bytes = compile_to_wasm(source)?;
    
    let imports = js_sys::Object::new();
    let console_obj = js_sys::Object::new();
    let env_obj = js_sys::Object::new();
    
    // Console.log writes to page output AND console
    let log_fn = js_sys::Function::new_with_args(
        "ptr, len",
        r#"
        try {
            const memory = globalThis.__rustscript_memory;
            if (memory && memory.buffer) {
                const bytes = new Uint8Array(memory.buffer, ptr, len);
                const text = new TextDecoder().decode(bytes);
                
                // Write to page output
                const outputEl = document.getElementById('output');
                if (outputEl) {
                    const line = document.createElement('div');
                    line.className = 'log';
                    line.textContent = text;
                    outputEl.appendChild(line);
                }
                
                // Also log to console
                console.log(text);
            }
        } catch (e) {
            console.error('[RustScript] Log error:', e);
        }
        "#
    );
    
    let error_fn = js_sys::Function::new_with_args(
        "ptr, len",
        r#"
        try {
            const memory = globalThis.__rustscript_memory;
            if (memory && memory.buffer) {
                const bytes = new Uint8Array(memory.buffer, ptr, len);
                const text = new TextDecoder().decode(bytes);
                
                // Write to page output
                const outputEl = document.getElementById('output');
                if (outputEl) {
                    const line = document.createElement('div');
                    line.className = 'error';
                    line.textContent = text;
                    outputEl.appendChild(line);
                }
                
                console.error(text);
            }
        } catch (e) {
            console.error('[RustScript] Error:', e);
        }
        "#
    );
    
    let malloc_fn = js_sys::Function::new_with_args(
        "size",
        r#"
        if (!globalThis.__rustscript_heap_ptr) globalThis.__rustscript_heap_ptr = 1024;
        const ptr = globalThis.__rustscript_heap_ptr;
        globalThis.__rustscript_heap_ptr += size;
        return ptr;
        "#
    );
    
    js_sys::Reflect::set(&console_obj, &"log".into(), &log_fn)?;
    js_sys::Reflect::set(&console_obj, &"error".into(), &error_fn)?;
    js_sys::Reflect::set(&env_obj, &"malloc".into(), &malloc_fn)?;
    js_sys::Reflect::set(&imports, &"console".into(), &console_obj)?;
    js_sys::Reflect::set(&imports, &"env".into(), &env_obj)?;
    
    let promise = js_sys::WebAssembly::instantiate_buffer(&wasm_bytes, &imports);
    let result = wasm_bindgen_futures::JsFuture::from(promise).await?;
    
    let instance = js_sys::Reflect::get(&result, &"instance".into())?;
    let exports = js_sys::Reflect::get(&instance, &"exports".into())?;
    
    if let Ok(memory) = js_sys::Reflect::get(&exports, &"memory".into()) {
        js_sys::Reflect::set(&js_sys::global(), &"__rustscript_memory".into(), &memory)?;
    }
    
    let main_fn = js_sys::Reflect::get(&exports, &"main".into())?;
    
    if main_fn.is_function() {
        let func = js_sys::Function::from(main_fn);
        func.call0(&JsValue::NULL)?;
    } else {
        log("Warning: No 'main' function found in script.");
    }
    
    Ok(())
}