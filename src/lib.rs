//! RustScript compiler library.
//!
//! Author: Michael Lauzon
//!
//! This module provides the WebAssembly interface for the RustScript compiler,
//! allowing RustScript programmes to be compiled and executed directly in web
//! browsers. It exposes functions via `wasm-bindgen` that can be called from
//! JavaScript.
//!
//! # Browser Usage
//!
//! The library is loaded as a WASM module and provides:
//! - `compile_to_wasm(source)` - Compiles RustScript source to WASM bytes
//! - `run_script(source)` - Compiles and executes a RustScript programme
//!
//! # Example
//!
//! ```javascript
//! import init, { run_script } from './pkg/RustScript.js';
//! await init();
//! await run_script('fn main() { console.log("Hello!"); }');
//! ```

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
pub mod tail_call_optimizer;
pub mod macro_system;
pub mod macro_registry;
pub mod macro_compiler;

// Import JavaScript console functions for logging.
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

/// Initialises the WASM module when it's first loaded.
///
/// Sets up a panic hook to display Rust panics in the browser console,
/// which is invaluable for debugging.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    Ok(())
}

/// Compiles RustScript source code to WebAssembly bytecode.
///
/// This function runs the full compilation pipeline:
/// 1. Parse the source into an AST
/// 2. Run the borrow checker for memory safety
/// 3. Run the type checker for type safety
/// 4. Generate WASM bytecode
///
/// # Arguments
///
/// * `source` - The RustScript source code to compile
///
/// # Returns
///
/// The compiled WASM binary as a byte vector, or a JavaScript error.
#[wasm_bindgen]
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, JsValue> {
    let mut ast = parser::parse_program(source)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {e}")))?;
    
    // Phase 4A: Tail call optimisation
    let mut tail_optimizer = tail_call_optimizer::TailCallOptimizer::new();
    tail_optimizer.optimize_module(&mut ast);
        
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

/// Compiles and executes a RustScript programme in the browser.
///
/// This is the main entry point for running RustScript in web pages. It:
/// 1. Compiles the source to WASM
/// 2. Sets up the import object with console and memory functions
/// 3. Instantiates the WASM module
/// 4. Calls the `main` function if it exists
///
/// Output from `console.log` is written to both the browser console and
/// any element with id="output" on the page.
///
/// # Arguments
///
/// * `source` - The RustScript source code to execute
#[wasm_bindgen]
pub async fn run_script(source: &str) -> Result<(), JsValue> {
    let wasm_bytes = compile_to_wasm(source)?;

    // Build the import object that provides host functions to WASM.
    let imports = js_sys::Object::new();
    let console_obj = js_sys::Object::new();
    let env_obj = js_sys::Object::new();
    
    // Console.log implementation: reads string from WASM memory and outputs it.
    // Writes to both the page output element and the browser console.
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
    
    // Console.error implementation: similar to log but styled as an error.
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
    
    // Simple bump allocator for WASM memory.
    // Starts at offset 1024 to leave room for static data.
    let malloc_fn = js_sys::Function::new_with_args(
        "size",
        r#"
        if (!globalThis.__rustscript_heap_ptr) globalThis.__rustscript_heap_ptr = 1024;
        const ptr = globalThis.__rustscript_heap_ptr;
        globalThis.__rustscript_heap_ptr += size;
        return ptr;
        "#
    );
    
    // Input function: prompts user for input and returns the string.
    // Uses window.prompt() for synchronous input in the browser.
    let input_fn = js_sys::Function::new_with_args(
        "ptr, len",
        r#"
        try {
            const memory = globalThis.__rustscript_memory;
            let promptText = "";
            if (memory && memory.buffer && len > 0) {
                const bytes = new Uint8Array(memory.buffer, ptr, len);
                promptText = new TextDecoder().decode(bytes);
            }
            
            // Use custom input handler if available, otherwise use prompt().
            let result = "";
            if (globalThis.__rustscript_input) {
                result = globalThis.__rustscript_input(promptText) || "";
            } else {
                result = window.prompt(promptText) || "";
            }
            
            // Write result to WASM memory and return pointer.
            const encoder = new TextEncoder();
            const encoded = encoder.encode(result);
            
            // Allocate memory for the result.
            if (!globalThis.__rustscript_heap_ptr) globalThis.__rustscript_heap_ptr = 1024;
            const resultPtr = globalThis.__rustscript_heap_ptr;
            globalThis.__rustscript_heap_ptr += encoded.length + 8;
            
            // Write length and string data.
            const view = new DataView(memory.buffer);
            view.setUint32(resultPtr, encoded.length, true);
            const resultBytes = new Uint8Array(memory.buffer, resultPtr + 4, encoded.length);
            resultBytes.set(encoded);
            
            return resultPtr;
        } catch (e) {
            console.error('[RustScript] Input error:', e);
            return 0;
        }
        "#
    );
    
    // Wire up the import object with our functions.
    js_sys::Reflect::set(&console_obj, &"log".into(), &log_fn)?;
    js_sys::Reflect::set(&console_obj, &"error".into(), &error_fn)?;
    js_sys::Reflect::set(&env_obj, &"malloc".into(), &malloc_fn)?;
    js_sys::Reflect::set(&env_obj, &"input".into(), &input_fn)?;
    js_sys::Reflect::set(&imports, &"console".into(), &console_obj)?;
    js_sys::Reflect::set(&imports, &"env".into(), &env_obj)?;

    // Instantiate the WASM module with our imports.
    let promise = js_sys::WebAssembly::instantiate_buffer(&wasm_bytes, &imports);
    let result = wasm_bindgen_futures::JsFuture::from(promise).await?;

    // Extract the instance and its exports.
    let instance = js_sys::Reflect::get(&result, &"instance".into())?;
    let exports = js_sys::Reflect::get(&instance, &"exports".into())?;

    // Store the WASM memory globally so console functions can access it.
    if let Ok(memory) = js_sys::Reflect::get(&exports, &"memory".into()) {
        js_sys::Reflect::set(&js_sys::global(), &"__rustscript_memory".into(), &memory)?;
    }

    // Call the main function if it exists.
    let main_fn = js_sys::Reflect::get(&exports, &"main".into())?;

    if main_fn.is_function() {
        let func = js_sys::Function::from(main_fn);
        func.call0(&JsValue::NULL)?;
    } else {
        log("Warning: No 'main' function found in script.");
    }

    Ok(())
}