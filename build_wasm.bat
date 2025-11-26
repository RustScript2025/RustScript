@echo off
echo Building RustScript for WebAssembly...

:: Check if wasm-pack is installed
where wasm-pack >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: wasm-pack is not installed.
    echo Please install it with: cargo install wasm-pack
    exit /b 1
)

:: Build the project
wasm-pack build --target web --out-dir www/pkg

if %errorlevel% equ 0 (
    echo Build successful!
    echo You can now serve the www directory.
) else (
    echo Build failed.
)
