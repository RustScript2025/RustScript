#!/bin/bash

# build_wasm.sh - Build RustScript WebAssembly runtime and examples
# This script builds the WebAssembly components needed to run RustScript in the browser

set -e  # Exit on any error

# Colours for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Colour

# Function to print coloured messages
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Print header
echo "========================================"
echo "  RustScript WebAssembly Build Script"
echo "========================================"
echo ""

# Check prerequisites
print_info "Checking prerequisites..."

if ! command_exists rustc; then
    print_error "Rust is not installed. Please install Rust from https://rustup.rs/"
    exit 1
fi

if ! command_exists cargo; then
    print_error "Cargo is not installed. Please install Rust from https://rustup.rs/"
    exit 1
fi

if ! command_exists wasm-pack; then
    print_error "wasm-pack is not installed."
    print_info "Installing wasm-pack..."
    cargo install wasm-pack
    if [ $? -ne 0 ]; then
        print_error "Failed to install wasm-pack"
        exit 1
    fi
    print_success "wasm-pack installed successfully"
fi

# Check Rust version
RUST_VERSION=$(rustc --version | cut -d' ' -f2)
print_success "Rust version: $RUST_VERSION"

# Check for Edition 2024 support (Rust 1.85+)
MAJOR=$(echo $RUST_VERSION | cut -d'.' -f1)
MINOR=$(echo $RUST_VERSION | cut -d'.' -f2)

if [ "$MAJOR" -eq 1 ] && [ "$MINOR" -lt 85 ]; then
    print_warning "Rust version $RUST_VERSION may not support Edition 2024"
    print_warning "Please update Rust: rustup update"
fi

echo ""

# Create www directory structure
print_info "Creating www directory structure..."
mkdir -p www
mkdir -p www/examples
mkdir -p www/lib
mkdir -p www/css
mkdir -p www/js

print_success "Directory structure created"
echo ""

# Build the RustScript compiler if not already built
if [ ! -f "target/release/rjsc" ]; then
    print_info "Building RustScript compiler..."
    cargo build --release
    if [ $? -ne 0 ]; then
        print_error "Failed to build RustScript compiler"
        exit 1
    fi
    print_success "RustScript compiler built successfully"
    echo ""
fi

# Build WebAssembly runtime from main project
print_info "Building WebAssembly runtime..."

wasm-pack build --target web --out-dir www/pkg
if [ $? -ne 0 ]; then
    print_error "Failed to build WebAssembly runtime"
    exit 1
fi
print_success "WebAssembly runtime built to www/pkg/"

echo ""

# Create a basic index.html if it doesn't exist
if [ ! -f "www/index.html" ]; then
    print_info "Creating default index.html..."
    cat > www/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>RustScript - WebAssembly Programming Language</title>
    <link rel="stylesheet" href="css/style.css">
</head>
<body>
    <header>
        <h1>RustScript</h1>
        <p>A modern programming language that compiles to WebAssembly</p>
    </header>
    
    <main>
        <section class="intro">
            <h2>Welcome to RustScript</h2>
            <p>RustScript is a web-focused programming language designed to run in browsers via WebAssembly.</p>
        </section>
        
        <section class="examples">
            <h2>Examples</h2>
            <ul>
                <li><a href="examples/hello.html">Hello World</a></li>
            </ul>
        </section>
        
        <section class="getting-started">
            <h2>Getting Started</h2>
            <p>Check out the <a href="https://github.com/yourusername/RustScript">documentation</a> to learn more.</p>
        </section>
    </main>
    
    <footer>
        <p>Built with RustScript</p>
    </footer>
</body>
</html>
EOF
    print_success "Created index.html"
fi

# Create a basic CSS file if it doesn't exist
if [ ! -f "www/css/style.css" ]; then
    print_info "Creating default style.css..."
    cat > www/css/style.css << 'EOF'
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
    line-height: 1.6;
    color: #333;
    background-color: #f4f4f4;
}

header {
    background: #2c3e50;
    color: #fff;
    padding: 2rem;
    text-align: center;
}

header h1 {
    margin-bottom: 0.5rem;
}

main {
    max-width: 1200px;
    margin: 2rem auto;
    padding: 0 2rem;
}

section {
    background: #fff;
    padding: 2rem;
    margin-bottom: 2rem;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

h2 {
    color: #2c3e50;
    margin-bottom: 1rem;
}

ul {
    list-style-position: inside;
}

a {
    color: #3498db;
    text-decoration: none;
}

a:hover {
    text-decoration: underline;
}

footer {
    text-align: center;
    padding: 2rem;
    color: #666;
}

#output {
    background: #f9f9f9;
    border: 1px solid #ddd;
    padding: 1rem;
    margin-top: 1rem;
    border-radius: 4px;
    font-family: 'Courier New', monospace;
}
EOF
    print_success "Created style.css"
fi

# Create a hello world example if it doesn't exist
if [ ! -f "www/examples/hello.html" ]; then
    print_info "Creating hello world example..."
    cat > www/examples/hello.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Hello World - RustScript</title>
    <link rel="stylesheet" href="../css/style.css">
</head>
<body>
    <header>
        <h1>RustScript: Hello World</h1>
        <p><a href="../index.html">← Back to Home</a></p>
    </header>
    
    <main>
        <section>
            <h2>Hello World Example</h2>
            <p>This is a simple "Hello, World!" programme written in RustScript.</p>
            
            <h3>Output:</h3>
            <div id="output">Loading...</div>
        </section>
        
        <section>
            <h3>Source Code:</h3>
            <pre><code>fn main() {
    println("Hello, World!");
}</code></pre>
        </section>
    </main>
    
    <script type="module">
        // Load and initialise the RustScript WebAssembly runtime
        // This will be populated with actual runtime loading code
        document.getElementById('output').textContent = 'Hello, World!';
    </script>
</body>
</html>
EOF
    print_success "Created hello world example"
fi

echo ""
print_success "Build complete!"
echo ""
print_info "To start the development server, run:"
echo "    python3 serve.py"
echo ""
print_info "Then open your browser to:"
echo "    http://localhost:8000"
echo ""
