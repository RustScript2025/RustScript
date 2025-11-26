@echo off
:: ============================================================================
:: RustScript WebAssembly Build Script
:: Author: Michael Lauzon
::
:: This script compiles the RustScript compiler to WebAssembly, enabling
:: RustScript programmes to be compiled and executed directly in web browsers.
::
:: Prerequisites:
::   - Rust toolchain (rustup, cargo)
::   - wasm-pack (install with: cargo install wasm-pack)
::
:: Output:
::   The compiled WASM package is placed in www/pkg/, ready to be served
::   by the development server (serve.py).
::
:: Usage:
::   build_wasm.bat
:: ============================================================================

echo Building RustScript for WebAssembly...

:: Verify that wasm-pack is installed before attempting the build.
where wasm-pack >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: wasm-pack is not installed.
    echo Please install it with: cargo install wasm-pack
    exit /b 1
)

:: Build the project targeting web browsers.
:: --target web: Generates ES modules for browser use.
:: --out-dir www/pkg: Places output in the www directory for serving.
wasm-pack build --target web --out-dir www/pkg

if %errorlevel% equ 0 (
    echo.
    echo Build successful!
    echo The WASM package has been generated in www/pkg/
    echo You can now run: python serve.py
) else (
    echo.
    echo Build failed. Check the error messages above.
)
