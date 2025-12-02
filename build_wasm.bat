@echo off
setlocal enabledelayedexpansion

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

echo ========================================
echo   RustScript WebAssembly Build Script
echo ========================================
echo.

:: Check prerequisites
echo [INFO] Checking prerequisites...

where rustc >nul 2>nul
if %errorlevel% neq 0 (
    echo [ERROR] Rust is not installed. Please install Rust from https://rustup.rs/
    exit /b 1
)

where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo [ERROR] Cargo is not installed. Please install Rust from https://rustup.rs/
    exit /b 1
)

where wasm-pack >nul 2>nul
if %errorlevel% neq 0 (
    echo [ERROR] wasm-pack is not installed.
    echo [INFO] Installing wasm-pack...
    cargo install wasm-pack
    if %errorlevel% neq 0 (
        echo [ERROR] Failed to install wasm-pack
        exit /b 1
    )
    echo [SUCCESS] wasm-pack installed successfully
)

:: Check Rust version
for /f "tokens=2" %%i in ('rustc --version') do set RUST_VERSION=%%i
echo [SUCCESS] Rust version: %RUST_VERSION%

:: Check for Edition 2024 support (Rust 1.85+)
for /f "tokens=1,2 delims=." %%a in ("%RUST_VERSION%") do (
    set MAJOR=%%a
    set MINOR=%%b
)

if %MINOR% LSS 85 (
    echo [WARNING] Rust version %RUST_VERSION% may not support Edition 2024
    echo [WARNING] Please update Rust: rustup update
)

echo.

:: Create www directory structure
echo [INFO] Creating www directory structure...
if not exist "www" mkdir www
if not exist "www\examples" mkdir www\examples
if not exist "www\lib" mkdir www\lib
if not exist "www\css" mkdir www\css
if not exist "www\js" mkdir www\js

echo [SUCCESS] Directory structure created
echo.

:: Build the RustScript compiler if not already built
if not exist "target\release\rsxe.exe" (
    echo [INFO] Building RustScript compiler...
    cargo build --release
    if %errorlevel% neq 0 (
        echo [ERROR] Failed to build RustScript compiler
        exit /b 1
    )
    echo [SUCCESS] RustScript compiler built successfully
    echo.
)

:: Build WebAssembly runtime from main project
echo [INFO] Building WebAssembly runtime...

wasm-pack build --target web --out-dir www\pkg
if %errorlevel% neq 0 (
    echo [ERROR] Failed to build WebAssembly runtime
    exit /b 1
)
echo [SUCCESS] WebAssembly runtime built to www\pkg\

echo.

:: Create a basic index.html if it doesn't exist
if not exist "www\index.html" (
    echo [INFO] Creating default index.html...
    (
        echo ^<!DOCTYPE html^>
        echo ^<html lang="en"^>
        echo ^<head^>
        echo     ^<meta charset="UTF-8"^>
        echo     ^<meta name="viewport" content="width=device-width, initial-scale=1.0"^>
        echo     ^<title^>RustScript - WebAssembly Programming Language^</title^>
        echo     ^<link rel="stylesheet" href="css/style.css"^>
        echo ^</head^>
        echo ^<body^>
        echo     ^<header^>
        echo         ^<h1^>RustScript^</h1^>
        echo         ^<p^>A modern programming language that compiles to WebAssembly^</p^>
        echo     ^</header^>
        echo.
        echo     ^<main^>
        echo         ^<section class="intro"^>
        echo             ^<h2^>Welcome to RustScript^</h2^>
        echo             ^<p^>RustScript is a web-focused programming language designed to run in browsers via WebAssembly.^</p^>
        echo         ^</section^>
        echo.
        echo         ^<section class="examples"^>
        echo             ^<h2^>Examples^</h2^>
        echo             ^<ul^>
        echo                 ^<li^>^<a href="examples/hello.html"^>Hello World^</a^>^</li^>
        echo             ^</ul^>
        echo         ^</section^>
        echo.
        echo         ^<section class="getting-started"^>
        echo             ^<h2^>Getting Started^</h2^>
        echo             ^<p^>Check out the ^<a href="https://github.com/RustScript2025/RustScript"^>documentation^</a^> to learn more.^</p^>
        echo         ^</section^>
        echo     ^</main^>
        echo.
        echo     ^<footer^>
        echo         ^<p^>Built with RustScript^</p^>
        echo     ^</footer^>
        echo ^</body^>
        echo ^</html^>
    ) > www\index.html
    echo [SUCCESS] Created index.html
)

:: Create a basic CSS file if it doesn't exist
if not exist "www\css\style.css" (
    echo [INFO] Creating default style.css...
    (
        echo * {
        echo     margin: 0;
        echo     padding: 0;
        echo     box-sizing: border-box;
        echo }
        echo.
        echo body {
        echo     font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
        echo     line-height: 1.6;
        echo     color: #333;
        echo     background-color: #f4f4f4;
        echo }
        echo.
        echo header {
        echo     background: #2c3e50;
        echo     color: #fff;
        echo     padding: 2rem;
        echo     text-align: center;
        echo }
        echo.
        echo header h1 {
        echo     margin-bottom: 0.5rem;
        echo }
        echo.
        echo main {
        echo     max-width: 1200px;
        echo     margin: 2rem auto;
        echo     padding: 0 2rem;
        echo }
        echo.
        echo section {
        echo     background: #fff;
        echo     padding: 2rem;
        echo     margin-bottom: 2rem;
        echo     border-radius: 8px;
        echo     box-shadow: 0 2px 4px rgba(0,0,0,0.1^);
        echo }
        echo.
        echo h2 {
        echo     color: #2c3e50;
        echo     margin-bottom: 1rem;
        echo }
        echo.
        echo ul {
        echo     list-style-position: inside;
        echo }
        echo.
        echo a {
        echo     color: #3498db;
        echo     text-decoration: none;
        echo }
        echo.
        echo a:hover {
        echo     text-decoration: underline;
        echo }
        echo.
        echo footer {
        echo     text-align: center;
        echo     padding: 2rem;
        echo     color: #666;
        echo }
        echo.
        echo #output {
        echo     background: #f9f9f9;
        echo     border: 1px solid #ddd;
        echo     padding: 1rem;
        echo     margin-top: 1rem;
        echo     border-radius: 4px;
        echo     font-family: 'Courier New', monospace;
        echo }
    ) > www\css\style.css
    echo [SUCCESS] Created style.css
)

:: Create a hello world example if it doesn't exist
if not exist "www\examples\hello.html" (
    echo [INFO] Creating hello world example...
    (
        echo ^<!DOCTYPE html^>
        echo ^<html lang="en"^>
        echo ^<head^>
        echo     ^<meta charset="UTF-8"^>
        echo     ^<meta name="viewport" content="width=device-width, initial-scale=1.0"^>
        echo     ^<title^>Hello World - RustScript^</title^>
        echo     ^<link rel="stylesheet" href="../css/style.css"^>
        echo ^</head^>
        echo ^<body^>
        echo     ^<header^>
        echo         ^<h1^>RustScript: Hello World^</h1^>
        echo         ^<p^>^<a href="../index.html"^>← Back to Home^</a^>^</p^>
        echo     ^</header^>
        echo.
        echo     ^<main^>
        echo         ^<section^>
        echo             ^<h2^>Hello World Example^</h2^>
        echo             ^<p^>This is a simple "Hello, World!" programme written in RustScript.^</p^>
        echo.
        echo             ^<h3^>Output:^</h3^>
        echo             ^<div id="output"^>Loading...^</div^>
        echo         ^</section^>
        echo.
        echo         ^<section^>
        echo             ^<h3^>Source Code:^</h3^>
        echo             ^<pre^>^<code^>fn main(^) {
        echo     println("Hello, World!"^);
        echo }^</code^>^</pre^>
        echo         ^</section^>
        echo     ^</main^>
        echo.
        echo     ^<script type="module"^>
        echo         // Load and initialise the RustScript WebAssembly runtime
        echo         // This will be populated with actual runtime loading code
        echo         document.getElementById('output'^).textContent = 'Hello, World!';
        echo     ^</script^>
        echo ^</body^>
        echo ^</html^>
    ) > www\examples\hello.html
    echo [SUCCESS] Created hello world example
)

echo.
echo [SUCCESS] Build complete!
echo.
echo [INFO] To start the development server, run:
echo     python serve.py
echo.
echo [INFO] Then open your browser to:
echo     http://localhost:8000
echo.

endlocal
