![Version](https://img.shields.io/badge/version-0.4.1-blue)
![Rust](https://img.shields.io/badge/rust-1.91.1%2B-orange)
![License](https://img.shields.io/badge/license-GPL--2.0-green)

<p align="center">
<img src="logo.jpg" alt="RustScript">
</p>

# RustScript

**Author**: Michael Lauzon

RustScript is a modern **web-scripting language** that compiles to WebAssembly for browser-based applications. It synthesises the best ideas from 60+ years of scripting language evolution, starting with Rust's memory safety and JavaScript's ergonomic syntax as a foundation, whilst incorporating powerful features from languages spanning LISP (1958) to modern languages (2025).

## Table of Contents

- [What's New in v0.4.1](#whats-new-in-v041)
- [Language Heritage](#language-heritage)
- [Design Philosophy](#design-philosophy)
- [Installation](#installation)
- [Browser Runtime](#browser-runtime)
- [Example Games](#example-games)
- [Usage](#usage)
- [Getting Started](#getting-started)
- [Common Issues for Beginners](#common-issues-for-beginners)
- [Language Guide & Examples](#language-guide--examples)
- [Scripting Language Features & Their Origins](#scripting-language-features--their-origins)
- [Contributing](#contributing)
- [Compiler Implementation](#compiler-implementation)
- [License](#license)
- [Phase 1 Features](#phase-1-features)
- [Phase 2 Features](#phase-2-features)
- [Phase 3 Features](#phase-3-features)
- [Phase 4 Features (NEW!)](#phase-4-features-new)
- [All Features Summary](#all-features-summary)
- [Feature Attribution](#feature-attribution)
- [Why These Features?](#why-these-features)
- [Standing on the Shoulders of Giants](#standing-on-the-shoulders-of-giants)
- [Further Reading](#further-reading)

## What's New in v0.4.1

### 🎉 MUSHcode-Inspired Features!

RustScript v0.4.1 adds **5 new features** inspired by MUSHcode/Softcode (1990), bringing the total to **87 features** from 60+ languages spanning 68 years (1958-2025).

**New in v0.4.1:**
- **Iteration Placeholders** (`##` for value, `#@` for index) - Concise iteration syntax
- **Register Variables** (`%q0`-`%q9`) - Fast temporary numeric storage
- **String Registers** (`%r0`-`%r9`) - Efficient string accumulation
- **Literal Operator** (`lit!()`) - Code-as-data patterns
- **Default Function** - Flexible fallback handling beyond null coalescing

### Previous: v0.4.0 - Phase 4 Complete!

RustScript v0.4.0 added **Phase 4: 72 advanced features** transforming RustScript from a modern scripting language into a production-ready systems programming language for the web.

### New Features: Phase 4

**Phase 4** adds 72 advanced features including:
- Memory safety (lifetimes, borrowing, move semantics, tail call optimisation)
- Advanced type system (union types, GADTs, higher-kinded types, refinement types)
- Functional programming (partial application, currying, lenses, transducers)
- Safe concurrency (async/await, channels, parallel iterators, atomic operations)
- Advanced control flow (try blocks, guard clauses, defer, labelled blocks)
- Metaprogramming (declarative macros, procedural macros, compile-time reflection)
- Domain-specific features (regex literals, operator overloading, destructuring)
- Developer utilities (default parameters, const functions, ranges with step)

*Inspired by*: Rust, Haskell, TypeScript, Scala, OCaml, F#, ML, Clojure, Julia, Go, Swift, Kotlin, Zig, D, Lisp, and many more.

### Documentation Updates
- 📚 New comprehensive [Phase 4 Features Documentation](docs/PHASE4_FEATURES.md)
- 📖 Enhanced [Tutorial](docs/TUTORIAL.md) with Phase 4 examples
- ⚡ Updated [Quick Reference](docs/QUICK_REFERENCE.md) with all 87 features

### Previous Releases

**v0.3.0** - Development Server & Compiler Improvements
- Feature-rich development server with hot reload, HTTPS, file upload, and more
- Fixed compiler hanging issue
- File extension change: `.rjsc` → `.rscc` (RustSCript Code)
- Output extension: `.rscx` (RustScript Code Xecutable)
- Compiler renamed: `rjsc` → `rsxe` (RustScript Xecutable Engine)

**Important**: RustScript is designed for **web development** (running in browsers via WebAssembly), not for shell scripting or command-line automation. If you're looking for a Rust-based tool to replace bash, Python scripts, or PowerShell for system administration tasks, RustScript is not the right tool. For shell scripting needs, consider writing regular Rust programmes with `cargo`, using script runners like `rust-script` or `cargo-script` for single-file scripts, using `evcxr` for interactive Rust REPL sessions, or sticking with traditional shell scripting languages.

## Language Heritage

RustScript draws inspiration from over 60 scripting languages across computing history:

**Expressiveness**: Python (1991), Ruby (1995), JavaScript (1995), Rebol (1997), Lua (1993), Tcl (1988), Perl (1987)  
**Functional Programming**: LISP (1958), Scheme (1975), Clojure (2007), Racket (1995), Emacs Lisp (1985)  
**Pattern Matching**: Erlang (1986), Elixir (2011), Prolog (1972)  
**Metaprogramming**: LISP (1958), Nim (2008), Julia (2012), Groovy (2003)  
**Multiple Dispatch**: Common Lisp CLOS (1988), Dylan (1992), Clojure (2007), Julia (2012)  
**Data Processing**: SNOBOL (1962), APL (1966), MUMPS (1966), Forth (1970), AWK (1977), Icon (1977), MUSHcode (1990), sed (1974), Awk (1977)  
**Type Systems**: TypeScript (2012), CoffeeScript (2009), Dart (2011)  
**Concurrency**: Erlang (1986), Elixir (2011), Go (2009)  
**Concurrency**: Erlang (1986), C++ atomics (2011), Elixir (2011), Go (2009), Rust (2010), Rayon (2016)  
**Modern Features**: Java (1995), C# (2000), Scala (2004), Rust (2010), Ceylon (2011), Kotlin (2011), TypeScript (2012), Swift (2014)  

## Design Philosophy

RustScript addresses a fundamental tension in modern software development: the trade-off between safety and productivity. Rather than forcing a choice between Rust's safety and JavaScript's ease of use, RustScript provides the best of both worlds—plus features from dozens of other scripting languages that solved specific problems elegantly.

### Core Principles

1.  **Gradual Safety** (from TypeScript, Kotlin): Types are optional during prototyping but can be added incrementally. Immutability is the default (from Rust, Clojure), catching entire classes of bugs without requiring explicit type annotations everywhere.

2.  **Expression-Oriented Design** (from Rust, Scala, ML): Following Rust's lead, nearly everything is an expression. This eliminates temporary variables and explicit return statements, resulting in more concise, functional code.

3.  **Composition Over Inheritance** (from Rust, Go): RustScript uses Rust's `struct` + `impl` model rather than class-based inheritance. This separation of data and behaviour encourages better architectural decisions and makes code easier to reason about.

4.  **Explicit Error Handling** (from Rust, Haskell): Rather than exceptions that create invisible control flow, RustScript uses `Result` types. Failure modes are explicit in function signatures, forcing deliberate error handling decisions.

5.  **Learn from History**: Every feature in RustScript has been proven in production across multiple languages. We don't reinvent the wheel—we use the best wheels ever made.

## Installation

### Building from Source

RustScript requires Rust 1.85 or later with Edition 2024 support.

**Checking Your Rust Version**: Run `rustc --version` to see your installed version. Any version 1.85 or higher includes Edition 2024 support. If your version is older, update Rust by running `rustup update`.

**On Windows (PowerShell or Command Prompt):**
```powershell
git clone https://github.com/RustScript2025/RustScript.git
cd RustScript
cargo build --release
```

**On Linux/Mac:**
```bash
git clone https://github.com/RustScript2025/RustScript.git
cd RustScript
cargo build --release
```

The compiler binary will be located at:
- **Windows**: `target\release\rsxe.exe`
- **Linux/Mac**: `target/release/rsxe`

**Note**: RustScript is built using Rust Edition 2024, taking advantage of the latest language features including implicit format arguments, improved error handling, and modern idioms.

### System Requirements

- Rust 1.85+ with cargo (Edition 2024 support)
  - Check your version: `rustc --version`
  - Update if needed: `rustup update`
- For WebAssembly builds: wasm-pack (`cargo install wasm-pack`)
- For browser testing: Python 3.x (for the development server)
- A modern web browser (Chrome, Firefox, Safari, or Edge)

## Browser Runtime

RustScript programmes execute in web browsers via WebAssembly. To set up your development environment:

### Quick Start

1. **Build the WebAssembly runtime:**
   
   **On Windows:**
   ```powershell
   build_wasm.bat
   ```
   
   **On Linux/Mac:**
   ```bash
   ./build_wasm.sh
   ```
   
   This creates the `www` directory structure and builds the necessary WebAssembly components.

2. **Start the development server:**
   
   **On Windows:**
   ```powershell
   python serve.py
   ```
   
   **On Linux/Mac:**
   ```bash
   python3 serve.py
   ```
   
   The server includes hot reload, directory listing, file upload, HTTPS, and more. See [SERVE.md](docs/SERVE.md) for all features and options.
   
   **Important**: The `serve.py` script must be run from the RustScript root directory (where the `www` folder exists).

3. **Open your browser:**
   Navigate to `http://localhost:8000`

The runtime automatically compiles and executes `<script type="text/rustscript">` tags, similar to how browsers handle JavaScript.

### Troubleshooting

**"Directory 'www' not found" error?**  
You need to run the build script first (`build_wasm.bat` on Windows or `./build_wasm.sh` on Linux/Mac) to create the WebAssembly runtime and `www` directory structure.

## Example Games

RustScript includes example games demonstrating the language's capabilities. **Hide and Seek**, a complete text adventure game, is available in four versions showcasing different aspects of RustScript:

### 🎮 [Hide and Seek v1](www/hideseek.html) - JavaScript Version
The original full-featured game implemented in JavaScript. Demonstrates the game design that was then ported to RustScript.
- **File**: `www/hideseek.html`
- **Features**: Complete game with 12 rooms, 27 hiding spots, 3 difficulty levels
- **Tech**: Pure JavaScript implementation

### 🦀 [Hide and Seek v2](www/hideseek_v2.html) - RustScript WASM Demo
Demonstrates RustScript code compiled to WebAssembly and running in the browser. Shows the `input()` and `console.log()` functions working via WASM.
- **File**: `www/hideseek_v2.html`
- **Features**: RustScript code in `<script type="text/rustscript">` tags
- **Tech**: Native RustScript → WASM compilation
- **Note**: Full game logic requires arrays/structs not yet in WASM backend

### 🎯 [Hide and Seek v3](www/hideseek_v3.html) - Full Playable Game
The complete playable game with async input system and all features.
- **File**: `www/hideseek_v3.html`
- **Features**: Full game, difficulty selection, scoring, hints, map
- **Tech**: JavaScript with RustScript-style async input

### ⭐ [Hide and Seek v4](www/hideseek_v4.html) - RustScript 0.4.1 Patterns
Showcases RustScript 0.4.1 coding patterns with enhanced visuals and live stats.
- **File**: `www/hideseek_v4.html`
- **Features**: Full game with pattern matching, destructuring, spread operator, immutable updates
- **Tech**: JavaScript demonstrating RustScript 0.4.1 idioms

### RustScript Source Code
The game logic is also available as pure RustScript source:
- **File**: `examples/hideseek.rscc`
- **Features**: Complete game implementation showcasing structs, pattern matching, loops, and more

To play, start the development server (`python3 serve.py`) and visit `http://localhost:8000`.

## Usage

Compile a RustScript file (`.rscc`) to JavaScript or WASM:

**On Windows:**
```powershell
# Compile to JavaScript (default)
.\target\release\rsxe.exe input.rscc

# Compile to WebAssembly
.\target\release\rsxe.exe input.rscc --target wasm

# Compile to executable
.\target\release\rsxe.exe input.rscc --output output.rscx
```

**On Linux/Mac:**
```bash
# Compile to JavaScript (default)
./target/release/rsxe input.rscc

# Compile to WebAssembly
./target/release/rsxe input.rscc --target wasm

# Compile to executable
./target/release/rsxe input.rscc --output output.rscx
```

## Getting Started

**New to RustScript?** Start with our comprehensive tutorial:

📚 **[Complete Tutorial](docs/TUTORIAL.md)** - Learn RustScript from Hello World to advanced features

The tutorial covers:
- Getting started and your first programme
- Variables, functions, and control flow
- Data structures and pattern matching
- **Phase 1**: String interpolation, optional chaining, null coalescing, list comprehensions
- **Phase 2**: Pattern matching in function heads, generators, multiple dispatch
- **Phase 3**: Design by contract, effect systems, compile-time execution
- **Phase 4**: 72 advanced features including lifetimes, borrowing, GADTs, async/await, macros, and more
- Real-world application examples

### Quick Reference

📖 **[Quick Reference](docs/QUICK_REFERENCE.md)** - Concise syntax guide for quick lookup

### Feature Documentation

- 📖 **[Phase 1 Features](docs/PHASE1_FEATURES.md)** - Modern syntax enhancements (4 features)
- 📖 **[Phase 2 Features](docs/PHASE2_FEATURES.md)** - Advanced function capabilities (3 features)
- 📖 **[Phase 3 Features](docs/PHASE3_FEATURES.md)** - Safety & metaprogramming (3 features)
- 📖 **[Phase 4 Features](docs/PHASE4_FEATURES.md)** - Production-ready advanced features (77 features)
- 📖 **[Feature Attribution](docs/FEATURE_ATTRIBUTION.md)** - Complete attribution for all 87 RustScript features with language origins

### Examples

💻 **[Examples Directory](examples/)** - Working code examples demonstrating all features

## Common Issues for Beginners

### "I thought RustScript was for shell scripting"
RustScript is focused on **web development** (browser-based applications that run via WebAssembly), not system scripting or command-line automation. The name can be misleading if you're expecting a bash or Python replacement.

**For shell scripting and command-line automation**, consider these Rust alternatives:
- **Regular Rust programmes with `cargo`** - Compile to native binaries for maximum performance
- **`rust-script`** - Run single `.rs` files as scripts without creating a full Cargo project (`cargo install rust-script`)
- **`cargo-script`** - Similar to `rust-script`, an older but stable alternative for running Rust as scripts (`cargo install cargo-script`)
- **`evcxr`** - Interactive Rust REPL for experimentation and quick scripting tasks (`cargo install evcxr_repl`)
- **Traditional shell scripting** - bash, Python, PowerShell, etc., which may be more appropriate for simple automation tasks

### "How do I know if I have Edition 2024 support?"
Run `rustc --version` in your terminal (PowerShell, Command Prompt, or bash). If the version number is 1.85 or higher, you have Edition 2024 support. For example:
```
rustc --version
rustc 1.91.1 (ed61e7d7e 2024-11-07)
```

This version (1.91.1) includes Edition 2024 support. If your version is lower than 1.85, update Rust:
```
rustup update
```

This command works the same on Windows, Linux, and Mac.

### "Directory 'www' not found when running serve.py"
The development server expects a `www` directory that doesn't exist until you build it.

**Solution:**
```bash
# On Windows
build_wasm.bat

# On Linux/Mac
./build_wasm.sh
```

This creates the `www` directory structure and builds the WebAssembly runtime. After running the build script, `python3 serve.py` will work correctly.

**Note**: Always run `serve.py` from the RustScript root directory (the directory containing the `www` folder).

### "I don't understand git/cargo/wasm-pack"
That's perfectly fine! You don't need to be an expert in these tools to learn RustScript. Here are some helpful resources:
- **Git basics**: [Official Git Book](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)
- **Cargo (Rust's package manager)**: [The Cargo Book](https://doc.rust-lang.org/cargo/)
- **wasm-pack**: Installed via `cargo install wasm-pack` — you don't need deep knowledge to use it

The important thing is following the step-by-step instructions in our tutorial. You'll pick up the necessary tool knowledge along the way.

## Language Guide & Examples

### 1. Variables and Types

RustScript uses `let` for immutable variables and `let mut` for mutable ones.

**Rationale**: JavaScript's `let` is mutable by default, requiring `const` for immutability. RustScript inverts this: `let` creates immutable bindings, whilst `let mut` explicitly opts into mutation. This design makes state changes visible and deliberate, reducing an entire category of bugs related to unexpected mutations.

```rustscript
// Immutable by default
// Attempting to reassign 'name' later would be a compile error.
let name = "Alice";

// Mutable variable
// We explicitly opt in to mutation.
let mut count: i32 = 0;
count += 1;

// Constants
// Compile-time constants, similar to Rust's const.
const MAX_USERS = 100;
```

### 2. Functions

Functions are expression-oriented.

**Rationale**: Implicit returns (the final expression in a block) encourage thinking about functions as data transformations rather than imperative procedures. Named arguments eliminate the "mystery boolean" problem where call sites like `greet("Bob", true)` are opaque compared to `greet(name: "Bob", formal: true)`.

```rustscript
fn add(a: i32, b: i32) -> i32 {
    a + b // Implicit return, no semicolon needed
}

// Named arguments supported
fn greet(name: string, formal: bool) {
    if formal {
        console.log("Good day, " + name);
    } else {
        console.log("Hi " + name);
    }
}

// Clearer call site
greet(name: "Bob", formal: false);
```

### 3. Structs and Objects

We use `struct` for data and `impl` for behaviour.

**Rationale**: Class-based inheritance often leads to deep hierarchies and tight coupling between state and behaviour. Structs are pure data structures. Methods are functions that operate on that data. This separation is fundamental to RustScript's design, promoting composition and making data flow explicit.

```rustscript
struct User {
    username: string,
    email: string,
    active: bool
}

impl User {
    // Static method (constructor convention)
    fn new(name: string) -> User {
        User {
            username: name,
            email: "",
            active: true
        }
    }

    // Method taking mutable reference to self
    fn deactivate(&mut this) {
        this.active = false;
    }
}
```

### 4. Pattern Matching

Powerful pattern matching for control flow.

**Rationale**: JavaScript's `switch` statements are error-prone due to implicit fallthrough and lack of exhaustiveness checking. Match expressions require handling all cases and support destructuring, eliminating an entire category of "unhandled state" bugs at compile time.

```rustscript
fn get_status_message(status: i32) -> string {
    match status {
        200 => "OK",
        404 => "Not Found",
        500..=599 => "Server Error", // Range matching
        _ => "Unknown Status"        // Catch-all
    }
}
```

### 5. Error Handling

RustScript uses `Result` and `Option` types.

**Rationale**: Exceptions create invisible control flow that can jump across arbitrary stack frames. `Result<T, E>` treats errors as first-class values that can be passed, transformed, and handled explicitly. Whilst `try/catch` remains available for JavaScript interoperability, idiomatic RustScript code uses `Result` types.

```rustscript
fn divide(a: f64, b: f64) -> Result<f64, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

// Usage
match divide(10.0, 2.0) {
    Ok(val) => console.log("Result: " + val),
    Err(msg) => console.error("Error: " + msg)
}
```

### 6. Advanced Features

RustScript includes modern features like the pipeline operator (`|>`), `defer`, and `guard`.

**Rationale**:
- **Pipeline (`|>`)**: Deeply nested function calls like `a(b(c(data)))` require reading inside-out. Pipelines `data |> c |> b |> a` follow natural left-to-right data flow, improving readability.
- **Guard**: Early returns for error conditions reduce nesting (the "arrow code" problem), making the happy path more prominent.
- **Defer**: Guarantees cleanup code executes regardless of how a function exits (return, error, etc.), preventing resource leaks without try-finally blocks.

```rustscript
// Pipeline operator
let result = data
    |> process
    |> validate
    |> save;

// Guard clause
fn process_email(email: string) {
    // If condition is false, execute the block (usually returns)
    guard email.contains("@") else {
        console.error("Invalid email");
        return;
    }
    
    // Defer execution until end of scope
    defer {
        console.log("Finished processing");
    }

    // ... proceed with valid email
}
```

### 7. Memory Safety

RustScript provides compile-time memory safety through borrowing and lifetimes.

**Rationale**: Memory bugs (use-after-free, dangling pointers, data races) are a major source of security vulnerabilities. RustScript's borrow checker ensures at compile time that references are always valid, without runtime overhead.

```rustscript
// Borrowing - multiple readers OR one writer
fn read_balance(account: &Account) -> number {
    account.balance  // Immutable borrow
}

fn deposit(account: &mut Account, amount: number) {
    account.balance += amount;  // Mutable borrow
}

// Lifetimes - compiler ensures references outlive their use
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length > y.length { x } else { y }
}
```

### 8. Functional Programming

RustScript supports advanced functional programming patterns.

**Rationale**: Functional patterns like partial application and composition enable building complex behaviour from simple, reusable pieces. These patterns reduce code duplication and make testing easier.

```rustscript
// Partial application - fix some arguments
fn add(a: number, b: number) -> number { a + b }
let add5 = add(5, _);
console.log(add5(10));  // 15

// Function composition - chain operations
fn add_one(x: number) -> number { x + 1 }
fn double(x: number) -> number { x * 2 }
let process = add_one >> double;
console.log(process(5));  // 12

// Currying - single-argument function chains
fn multiply(a: number)(b: number) -> number { a * b }
let double = multiply(2);
console.log(double(5));  // 10
```

### 9. Async Programming

RustScript provides async/await for non-blocking operations.

**Rationale**: Modern web applications require handling many concurrent operations efficiently. Async/await provides a clean syntax for asynchronous code that reads like synchronous code, avoiding callback hell.

```rustscript
async fn fetch_user(id: number) -> User {
    let response = await http.get("/api/users/{id}");
    await response.json()
}

async fn main() {
    let user = await fetch_user(42);
    console.log("Got user: {user.name}");
}
```

### 10. Metaprogramming

RustScript supports compile-time code generation and macros.

**Rationale**: Metaprogramming eliminates boilerplate by generating repetitive code at compile time. This keeps source code DRY whilst maintaining runtime performance.

```rustscript
// Compile-time execution
comptime {
    const BUFFER_SIZE = 1024;
    const FIB_10 = fibonacci(10);  // Computed at compile time
}

// Declarative macros
macro_rules! vec {
    ($($elem:expr),*) => {
        {
            let mut v = Vec::new();
            $(v.push($elem);)*
            v
        }
    }
}

let numbers = vec![1, 2, 3, 4, 5];
```

## Scripting Language Features & Their Origins

RustScript's feature set represents a curated selection from scripting language history:

### From the 1950s-1960s
- **LISP (1958)**: First-class functions, REPL, homoiconicity
- **SNOBOL (1962)**: Pattern matching and string manipulation, data processing
- **APL (1966)**: Array-oriented operations
- **MUMPS (1966)**: Persistent data structures

### From the 1970s
- **Forth (1970)**: Stack-based operations, concatenative programming
- **ML (1973)**: Type inference, algebraic data types
- **Scheme (1975)**: Lexical scoping, tail call optimisation
- **AWK (1977)**: Pattern-action paradigm, implicit iteration
- **Icon (1977)**: Generators, goal-directed evaluation

### From the 1980s
- **Ada (1983)**: Design by Contract, strong typing
- **Common Lisp (1984)**: Standardised Lisp dialect
- **Eiffel (1986)**: Design by Contract (requires/ensures/invariant)
- **Erlang (1986)**: Pattern matching in function heads, actor model
- **Common Lisp Object System (CLOS) (1988)**: Multiple dispatch, method combinations

### From the 1990s
- **Haskell (1990)**: Pure functional programming, lazy evaluation, advanced type systems
- **Python (1991)**: List comprehensions, generators, clean syntax
- **Dylan (1992)**: Multiple dispatch, object-oriented functional programming
- **Ruby (1995)**: String interpolation, blocks, method chaining
- **JavaScript (1995)**: Dynamic typing, first-class functions, prototypal inheritance
- **OCaml (1996)**: Functional and object-oriented programming, pattern matching
- **Rebol (1997)**: Dialects, minimal syntax

### From the 2000s
- **C# (2000)**: Null coalescing operator, LINQ
- **D (2001)**: Compile-time function execution, metaprogramming, contracts
- **Scala (2004)**: Hybrid functional/OOP, pattern matching
- **F# (2005)**: Computation expressions, type providers
- **Clojure (2007)**: Immutable data structures, persistent collections
- **Nim (2008)**: Effect system, metaprogramming

### From the 2010s
- **Kotlin (2011)**: Null safety, extension functions
- **Elixir (2011)**: Pattern matching, functional programming on BEAM VM
- **Julia (2012)**: Multiple dispatch, performance
- **TypeScript (2012)**: Gradual typing, structural types
- **Koka (2012)**: Effect system, algebraic effects
- **Swift (2014)**: Optional chaining, protocol extensions
- **Zig (2016)**: Compile-time execution (comptime)

## Contributing

Interested in contributing to RustScript? We'd love your help!

👉 **[Read the Contributing Guide](CONTRIBUTING.md)** for:
- Development setup and workflow
- Code style guidelines (Rust Edition 2024)
- How to add new features
- Testing and documentation standards
- Pull request process

## Compiler Implementation

The RustScript compiler is built with modern Rust practices:

- **Edition 2024**: Uses the latest Rust edition with implicit format arguments and modern idioms
- **Type Safety**: Comprehensive type checking with inference
- **Borrow Checking**: Memory safety without garbage collection
- **Error Handling**: Detailed diagnostics with source context and colour output
- **WebAssembly**: Direct compilation to WASM for browser execution
- **Modular Architecture**: Clean separation between lexer, parser, type checker, and code generator

### Compiler Pipeline

1. **Lexing**: Tokenises source code using the `logos` crate
2. **Parsing**: Builds AST using the Pest parser generator
3. **Type Checking**: Infers and validates types throughout the programme
4. **Borrow Checking**: Ensures memory safety and ownership rules
5. **Code Generation**: Produces JavaScript or WebAssembly output
6. **Source Maps**: Generates source maps for debugging

## License

This project is licensed under the GPL-2.0 Licence.


## Phase 1 Features

RustScript now includes **87 advanced features** across 4 major phases, inspired by 60+ languages spanning 68 years:

### String Interpolation
```rust
let name = "Alice";
let greeting = "Hello, {name}! Welcome to RustScript.";
```

### Optional Chaining
```rust
let street = user?.address?.street;  // Safe navigation
```

### Null Coalescing
```rust
let name = user_name ?? "Anonymous";  // Default values
```

### List Comprehensions
```rust
let evens = [x * 2 for x in numbers if x % 2 == 0];
```

See [Phase 1 Features Documentation](docs/PHASE1_FEATURES.md) for complete details and examples.


## Phase 2 Features

Advanced function capabilities inspired by the best functional languages:

### Pattern Matching in Function Heads
```rust
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }
```

### Generators
```rust
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}
```

### Multiple Dispatch
```rust
fn process(x: number, y: number) -> string { "Adding: {x + y}" }
fn process(x: string, y: string) -> string { "Concat: {x}{y}" }
```

See [Phase 2 Features Documentation](docs/PHASE2_FEATURES.md) for complete details and examples.


## Phase 3 Features

Safety and metaprogramming inspired by Eiffel, Zig, and modern effect systems:

### Design by Contract
```rust
fn divide(a: number, b: number) -> number
    requires b != 0, "Divisor cannot be zero"
    ensures result * b ≈ a
{
    a / b
}
```

### Effect System
```rust
effect [pure]
fn add(a: number, b: number) -> number { a + b }

effect [io, throws]
fn read_file(path: string) -> string { ... }
```

### Compile-time Execution
```rust
comptime {
    const BUFFER_SIZE = 1024;
    const FIB_10 = fibonacci(10);  // Computed at compile time
}
```

See [Phase 3 Features Documentation](docs/PHASE3_FEATURES.md) for complete details.


## Phase 4 Features (NEW!)

Advanced features bringing RustScript to production-ready status, inspired by Rust, Haskell, and modern systems languages:

### Lifetimes
```rust
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length > y.length { x } else { y }
}
```

### Borrowing
```rust
fn read(data: &Account) -> number { data.balance }
fn write(data: &mut Account, amount: number) { data.balance += amount; }
```

### Union Types
```rust
type StringOrNumber = string | number;

fn process(value: StringOrNumber) -> string {
    match value {
        string(s) => "Got string: {s}",
        number(n) => "Got number: {n}",
    }
}
```

### Async/Await
```rust
async fn fetch_user(id: number) -> Result<User, Error> {
    let response = await http.get("https://api.example.com/users/{id}")?;
    Ok(response.json())
}
```

### Channels
```rust
let (tx, rx) = channel();

spawn {
    for i in 0..5 {
        tx.send(i);
    }
};

for _ in 0..5 {
    console.log(rx.recv());
}
```

### Declarative Macros
```rust
macro_rules! vec {
    ($($x:expr),*) => {
        {
            let mut temp_vec = Vec::new();
            $(temp_vec.push($x);)*
            temp_vec
        }
    };
}

let numbers = vec!(1, 2, 3, 4, 5);
```

### Operator Overloading
```rust
struct Vec2 { x: number, y: number }

impl Add for Vec2 {
    fn add(self, other: Vec2) -> Vec2 {
        Vec2 { x: self.x + other.x, y: self.y + other.y }
    }
}

let sum = v1 + v2;  // Uses custom Add implementation
```

### Default Parameters
```rust
fn greet(name: string = "World", formal: bool = false) {
    if formal {
        console.log("Good day, {name}!");
    } else {
        console.log("Hi, {name}!");
    }
}

greet();                    // "Hi, World!"
greet("Alice");             // "Hi, Alice!"
greet("Bob", true);         // "Good day, Bob!"
```

See [Phase 4 Features Documentation](docs/PHASE4_FEATURES.md) for complete details.

## All Features Summary

RustScript now includes **82 advanced features** across 4 phases:

**Phase 1 - Syntax (4 features):** String interpolation, Optional chaining, Null coalescing, List comprehensions  
**Phase 2 - Functions (3 features):** Pattern matching in function heads, Generators, Multiple dispatch  
**Phase 3 - Safety (3 features):** Design by Contract, Effect system, Compile-time execution  
**Phase 4 - Advanced (77 features):** Lifetimes, Borrowing, Move semantics, Tail call optimisation, Pattern guards, Traits, Const generics, Algebraic effects, Inline assembly, RAII, Union/intersection types, Type aliases, Newtype pattern, Associated types, Higher-kinded types, Phantom types, Refinement types, Dependent types, Type-level programming, Existential types, GADTs, Variance, Type bounds, Subtyping, Structural typing, Partial application, Function composition, Currying, Lazy evaluation, Memoisation, Immutable data structures, Transducers, Do-notation, Applicative functors, Lenses, Async/await, Channels, Futures & streams, Mutex & RwLock, Atomic operations, Parallel iterators, Scoped threads, Select, Try blocks, Try operator (?), Guard clauses, Labelled blocks, Catch expressions, Panic, Defer, Conditional compilation, Const assertions, Unreachable, Declarative macros, Procedural macros, Attribute macros, Reflection, Code generation, Quasiquoting, Hygiene, Syntax extensions, Regex literals, Format strings, String slicing, Operator overloading, Custom indexing, Destructuring, Ranges with step, Zip iterator, Enumerate, Default parameters, Const functions, Iteration placeholders, Register variables, String registers, Literal operator, Default function

Drawing inspiration from 60+ languages spanning LISP (1958) to modern languages (2025).


## Feature Attribution

Each RustScript feature has a rich scripting language heritage:

### Phase 1: String & Syntax Enhancements (4 features)

**String Interpolation** (`"Hello, {name}!"`)
- Inspired by: Ruby (1995), Kotlin (2011), Python (2015), JavaScript ES6 (2015)
- Why: Eliminates error-prone string concatenation

**Optional Chaining** (`user?.address?.street`)
- Inspired by: Swift (2014), C# (2015), TypeScript (2020)
- Why: Safe navigation through potentially null values

**Null Coalescing** (`value ?? "default"`)
- Inspired by: C# (2000), PHP (2009), Swift (2014), JavaScript (2020)
- Why: Concise default value handling

**List Comprehensions** (`[x * 2 for x in numbers if x > 0]`)
- Inspired by: Haskell (1990), Python (1994), Scala (2004), F# (2005)
- Why: Declarative collection transformation

### Phase 2: Function Enhancements (3 features)

**Pattern Matching in Function Heads** (Multiple definitions)
- Inspired by: ML (1973), Erlang (1986), Haskell (1990), Elixir (2011)
- Why: Elegant handling of different input cases

**Generators** (`gen fn name() { yield value; }`)
- Inspired by: Icon (1977), Python (2001), C# (2005), JavaScript ES6 (2015)
- Why: Memory-efficient lazy evaluation

**Multiple Dispatch** (Type-based function selection)
- Inspired by: Common Lisp CLOS (1988), Dylan (1992), Clojure (2009), Julia (2012)
- Why: Symmetric treatment of all arguments

### Phase 3: Safety & Contracts (3 features)

**Design by Contract** (`requires`/`ensures`/`invariant`)
- Inspired by: Ada (1983), Eiffel (1986), D (2001), Spec# (2004)
- Why: Formal specification of function behaviour

**Effect System** (`effect [pure, io, state, ...]`)
- Inspired by: Nim (2008), Rust traits (2010), Eff (2012), Koka (2012)
- Why: Track and control side effects

**Compile-time Execution** (`comptime { ... }`)
- Inspired by: D CTFE (2007), Nim (2008), C++ constexpr (2011), Zig (2016)
- Why: Move computation from runtime to compile time

### Phase 4: Advanced Language Features (77 features)

**Lifetimes** - Cyclone (2002), Rust (2010)
**Borrowing & References** - Rust (2010)
**Move Semantics** - C++ (2011), Rust (2010)
**Tail Call Optimisation** - Scheme (1975), Lua (1993)
**Pattern Guards** - Erlang (1986), Haskell (1990)
**Traits (RAII & Drop)** - C++ RAII (1984), Rust (2010)
**Const Generics** - C++ templates (1990), Rust (2020)
**Algebraic Effects** - Eff (2012), Koka (2012)
**Inline Assembly** - C (1972), Rust (2015)
**Complete Trait System** - Haskell type classes (1988), Rust (2010)
**Union & Intersection Types** - Ceylon (2011), TypeScript (2012)
**Type Aliases** - Haskell (1990), Rust (2010), TypeScript (2012)
**Newtype Pattern** - Haskell (1990), Rust (2010)
**Associated Types** - Haskell (1996), Rust (2015)
**Higher-Kinded Types** - Haskell (1990), Scala (2004)
**Phantom Types** - Haskell (1990), Rust (2010)
**Refinement Types** - Liquid Haskell (2008), F* (2011)
**Dependent Types** - Coq (1989), Agda (2007), Idris (2007)
**Type-Level Programming** - Haskell (1990), TypeScript (2012)
**Existential Types** - Haskell (1990), Rust impl Trait (2018)
**GADTs** - Haskell (2003), OCaml (2004)
**Variance Annotations** - Scala (2004), Kotlin (2011)
**Type Bounds** - Haskell (1988), Java (2004), Rust (2010)
**Subtyping** - Scala (2004), TypeScript (2012)
**Structural Typing** - Go (2009), TypeScript (2012)
**Partial Application** - ML (1973), Haskell (1990), F# (2005)
**Function Composition** - Haskell (1990), F# (2005), Elixir (2011)
**Currying** - ML (1973), Haskell (1990), OCaml (1996)
**Lazy Evaluation** - Miranda (1985), Haskell (1990)
**Memoisation** - Common Lisp (1984), Python decorators (2004)
**Immutable Data Structures** - Scala (2004), Clojure (2007)
**Transducers** - Clojure (2014)
**Do-Notation** - Haskell (1990)
**Applicative Functors** - Haskell (2008)
**Lenses** - Haskell (2012)
**Async/Await** - C# (2012), JavaScript (2017), Rust (2019)
**Channels** - Go (2009), Rust (2015)
**Futures & Streams** - Scala (2010), Rust (2016)
**Mutex & RwLock** - Rust (2010), C++ (2011)
**Atomic Operations** - C++ (2011), Rust (2015)
**Parallel Iterators** - Rayon/Rust (2016)
**Scoped Threads** - Rust (2022)
**Select** - Go (2009), Rust (2015)
**Try Blocks** - Kotlin (2011), Rust (2018)
**Try Operator (?)** - Swift (2014), Rust (2016)
**Guard Clauses** - Ruby (1995), Swift (2014)
**Labelled Blocks** - Java (1995), Rust (2015)
**Catch Expressions** - Scala (2004), Kotlin (2011)
**Panic with Backtraces** - Go (2009), Rust (2015)
**Defer Statements** - Go (2009), Swift (2014), Zig (2016)
**Conditional Compilation** - C (1972), Rust (2015)
**Const Assertions** - C++ (2011), Rust (2019)
**Unreachable Markers** - Swift (2014), Rust (2015)
**Declarative Macros** - Lisp (1958), Rust macro_rules! (2015)
**Procedural Macros** - Lisp (1958), Rust (2018)
**Attribute Macros** - Java annotations (2004), Rust (2018)
**Compile-Time Reflection** - D (2001), Zig (2016)
**Code Generation** - D CTFE (2007), Zig comptime (2016)
**Quasiquoting** - Lisp (1960s), Rust quote! (2016)
**Hygiene** - Scheme (1986), Rust (2015)
**Syntax Extensions** - Scala (2004), Nim (2008), Rust (2015)
**Regex Literals** - Perl (1987), JavaScript (1995), Ruby (1995)
**Format Strings** - Python f-strings (2015), Rust (2018)
**String Slicing** - Python (1991), Rust (2015)
**Operator Overloading** - C++ (1983), Python (1991), Rust (2015)
**Custom Indexing** - C++ (1983), Python (1991), Rust (2015)
**Destructuring Assignment** - Python (1991), JavaScript ES6 (2015), Rust (2015)
**Ranges with Step** - Python (1991), Ruby (1995), Rust (2015)
**Zip Iterator** - Haskell (1990), Python (1991), Rust (2015)
**Enumerate** - Python (1991), Rust (2015)
**Default Parameters** - Python (1991), JavaScript (2015), Rust (2021)
**Const Functions** - C++ constexpr (2011), Rust (2018)

## Why These Features?

Each feature was chosen because it solved a real problem elegantly in its original language:

**Phases 1-3 (10 features):**
1. **String Interpolation** (Ruby 1995, Kotlin 2011, Swift 2014, Python 2015): Embedded expressions are more readable than concatenation
2. **Optional Chaining** (Swift 2014, C# 2015, Kotlin 2011, TypeScript 2020): Safe navigation prevents null pointer errors
3. **Null Coalescing** (C# 2000, PHP 2009, Swift 2014, JavaScript 2020): Default values without verbose if-else chains
4. **List Comprehensions** (Haskell 1990, Python 1994, Scala 2004, F# 2005): Transform collections with mathematical clarity
5. **Pattern Matching in Function Heads** (ML 1973, Erlang 1986, Haskell 1990, Elixir 2011): Multiple definitions clearer than nested if-else
6. **Generators** (Icon 1977, Python 2001, C# 2005, JavaScript ES6 2015): Lazy evaluation enables infinite sequences with finite memory
7. **Multiple Dispatch** (Common Lisp CLOS 1988, Dylan 1992, Clojure 2009, Julia 2012): Symmetric treatment of all arguments
8. **Design by Contract** (Ada 1983, Eiffel 1986, D 2001, Spec# 2004): Formal specifications catch bugs that tests miss
9. **Effect System** (Nim 2008, Rust traits 2010, Eff 2012, Koka 2012): Track side effects for easier reasoning
10. **Compile-time Execution** (D CTFE 2007, Nim 2008, C++ constexpr 2011, Zig 2016): Move work from runtime to compile time

**Phase 4 (77 features):**
- **Memory Safety** (Scheme TCO 1975, C++ RAII 1984, Cyclone 2002, Rust 2010): Scheme introduced tail call optimisation; C++ established RAII patterns; Cyclone pioneered region-based memory; Rust proved that memory safety without garbage collection is practical and performant
- **Advanced Types** (Coq 1989, Haskell 1990, OCaml 2004, Scala 2004, Agda 2007, Idris 2007, Liquid Haskell 2008, Ceylon 2011, F* 2011, TypeScript 2012): Sophisticated type systems catch bugs at compile time; dependent types from Idris/Agda enable proofs; refinement types from Liquid Haskell add constraints; GADTs from Haskell/OCaml enable type-safe DSLs
- **Functional Programming** (ML 1973, Common Lisp 1984, Miranda 1985, Haskell 1990, OCaml 1996, Scala 2004, F# 2005, Clojure 2007): Pure functions and immutability lead to more reliable code; Clojure's persistent data structures enable efficient immutability; Haskell's lenses provide composable data access
- **Concurrency** (Go 2009, Scala 2010, C++ 2011, C# 2012, Rust 2015, Rayon 2016, JavaScript 2017): Go's channels and Rust's ownership model showed that safe concurrency is achievable; async/await from C#/JavaScript simplified asynchronous code; Rayon demonstrated data parallelism
- **Control Flow** (Java 1995, Ruby 1995, Go 2009, Kotlin 2011, Swift 2014, Rust 2016, Zig 2016): Swift's guard clauses and Rust's ? operator proved that explicit error handling is clearer than exceptions; Go's defer ensures cleanup; labelled blocks from Java/Rust enable structured exits
- **Metaprogramming** (Lisp 1958, Scheme 1986, D 2001, Java 2004, Scala 2004, Nim 2008, Rust 2015, Zig 2016): Lisp invented macros; Rust refined them with hygiene from Scheme; Zig's comptime enables arbitrary compile-time computation; Java's annotations inspired attribute macros
- **Domain Features** (C++ 1983, Perl 1987, Python 1991, JavaScript 1995, Ruby 1995, Rust 2015): C++ introduced operator overloading; Perl pioneered regex literals; Python established string slicing and format strings; JavaScript/Rust refined destructuring
- **Utilities** (Haskell 1990, Python 1991, Ruby 1995, C++ 2011, JavaScript 2015, Rust 2015): Python's enumerate/zip/ranges became standard patterns; Rust's const functions and C++ constexpr enable compile-time evaluation; default parameters from Python/JavaScript improve API ergonomics

## Standing on the Shoulders of Giants

RustScript doesn't claim to invent new concepts. Instead, it carefully selects and integrates the best ideas from decades of programming language research and practice. Every feature has been battle-tested in production systems across multiple languages.

With 87 features drawn from over 60 languages spanning LISP (1958) to modern languages (2025), RustScript represents a curated synthesis of proven solutions. Each feature was chosen because it solved a real problem elegantly in production systems.

By learning from language history, RustScript avoids repeating past mistakes whilst embracing proven solutions. The result is a language that feels familiar to developers from many backgrounds whilst offering a cohesive, modern development experience.

## Further Reading

- 📚 [Tutorial](docs/TUTORIAL.md) - Complete guide from Hello World to advanced features
- 🚀 [Development Server](docs/SERVE.md) - Feature-rich server with hot reload, HTTPS, and more
- ⚡ [Quick Reference](docs/QUICK_REFERENCE.md) - Concise syntax reference
- 📖 [Phase 1 Features](docs/PHASE1_FEATURES.md) - String & syntax enhancements
- 📖 [Phase 2 Features](docs/PHASE2_FEATURES.md) - Function enhancements
- 📖 [Phase 3 Features](docs/PHASE3_FEATURES.md) - Safety & metaprogramming
- 📖 [Phase 4 Features](docs/PHASE4_FEATURES.md) - Advanced language features (77 features)
- 📖 [Feature Attribution](docs/FEATURE_ATTRIBUTION.md) - Complete attribution for all 87 RustScript features with language origins
- 💻 [Examples Directory](examples/) - Working code examples ([see examples README](examples/README.md))
- 🤝 [Contributing Guide](CONTRIBUTING.md) - How to contribute to RustScript


