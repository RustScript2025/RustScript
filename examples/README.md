# RustScript Examples

**Author**: Michael Lauzon

This directory contains example programmes demonstrating RustScript features.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Feature Examples](#feature-examples)
3. [Example Structure](#example-structure)
4. [Learning Path](#learning-path)
5. [Compiling to WebAssembly](#compiling-to-webassembly)
6. [Running in the Browser](#running-in-the-browser)
7. [Contributing Examples](#contributing-examples)
8. [Example Categories](#example-categories)
9. [Need Help?](#need-help)

---

## Getting Started

### Hello World

The simplest RustScript programme:

```rustscript
fn main() {
    console.log("Hello, World!");
}

main();
```

**On Windows:**
```powershell
..\target\release\rsxe.exe hello_world.rscc
node hello_world.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe hello_world.rscc
node hello_world.js
```

## Feature Examples

### Phase 1: String & Syntax Enhancements

**File**: `phase1_features.rscc`

Demonstrates:
- String interpolation with `{expression}` syntax
- Optional chaining with `?.` operator
- Null coalescing with `??` operator
- List comprehensions for collection transformation

```rustscript
// String interpolation
let name = "Alice";
let greeting = "Hello, {name}!";

// Optional chaining
let street = user?.address?.street;

// Null coalescing
let display_name = username ?? "Anonymous";

// List comprehensions
let squares = [x * x for x in numbers if x > 0];
```

**Run it:**

**On Windows:**
```powershell
..\target\release\rsxe.exe phase1_features.rscc
node phase1_features.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe phase1_features.rscc
node phase1_features.js
```

### Phase 2: Function Enhancements

**File**: `phase2_features.rscc`

Demonstrates:
- Pattern matching in function heads (Erlang-style)
- Generators with `yield` keyword
- Multiple dispatch (Julia-style)
- Lazy evaluation and infinite sequences

```rustscript
// Pattern matching in function heads
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }

// Generators
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}

// Multiple dispatch
fn process(x: number, y: number) -> string { "Adding: {x + y}" }
fn process(x: string, y: string) -> string { "Concat: {x}{y}" }
```

**Run it:**

**On Windows:**
```powershell
..\target\release\rsxe.exe phase2_features.rscc
node phase2_features.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe phase2_features.rscc
node phase2_features.js
```

### Phase 3: Safety & Metaprogramming

**File**: `phase3_features.rscc`

Demonstrates:
- Design by Contract (`requires`, `ensures`, `invariant`)
- Effect system (`effect [pure]`, `effect [io]`, etc.)
- Compile-time execution with `comptime` blocks
- Formal verification and safety guarantees

```rustscript
// Design by Contract
fn divide(a: number, b: number) -> number
    requires b != 0, "Divisor cannot be zero"
    ensures result * b ≈ a
{
    a / b
}

// Effect System
effect [pure]
fn add(a: number, b: number) -> number { a + b }

effect [io, throws]
fn read_file(path: string) -> string { /* ... */ }

// Compile-time Execution
comptime {
    const BUFFER_SIZE = 1024;
    const FIB_10 = fibonacci(10);  // Computed at compile time
}
```

**Run it:**

**On Windows:**
```powershell
..\target\release\rsxe.exe phase3_features.rscc
node phase3_features.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe phase3_features.rscc
node phase3_features.js
```

### Phase 4: Advanced Language Features

**File**: `phase4_features.rscc`

Demonstrates 77 advanced features across 9 categories:
- Memory safety (lifetimes, borrowing, tail call optimisation)
- Advanced types (union types, GADTs, higher-kinded types)
- Functional programming (partial application, currying, lenses)
- Concurrency (async/await, channels, parallel iterators)
- Control flow (try blocks, guard clauses, defer)
- Metaprogramming (macros, reflection, code generation)
- Domain-specific (regex literals, operator overloading)
- Utilities (default parameters, const functions)
- MUSHcode-inspired (iteration placeholders, registers, default function)

```rustscript
// Lifetimes
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length > y.length { x } else { y }
}

// Partial Application
let add5 = add(5, _);
console.log(add5(10));  // 15

// Async/Await
async fn fetch_user(id: number) -> User {
    let response = await http.get("/api/users/{id}");
    await response.json()
}

// Channels
let (tx, rx) = channel();
spawn { tx.send(42); };
let msg = rx.recv();

// Declarative Macros
macro_rules! vec {
    ($($x:expr),*) => {
        let mut v = Vec::new();
        $(v.push($x);)*
        v
    };
}
```

**Run it:**

**On Windows:**
```powershell
..\target\release\rsxe.exe phase4_features.rscc
node phase4_features.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe phase4_features.rscc
node phase4_features.js
```

### Hide and Seek Game

**File**: `hideseek.rscc`

A complete text adventure game demonstrating RustScript in action:
- Structs and data modelling
- Pattern matching
- Control flow
- User input handling
- Game state management

```rustscript
struct Room {
    name: string,
    description: string,
    hiding_spots: [string],
}

struct GameState {
    current_room: Room,
    seeker_location: string,
    found: bool,
}

fn check_hiding_spot(state: GameState, spot: string) -> GameState {
    if spot == state.seeker_location {
        console.log("Found them!");
        GameState { ...state, found: true }
    } else {
        console.log("Not here...");
        state
    }
}
```

**Run it:**

**On Windows:**
```powershell
..\target\release\rsxe.exe hideseek.rscc
node hideseek.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe hideseek.rscc
node hideseek.js
```

## Example Structure

Each example file follows this structure:

1. **Header Comment**: Describes what the file demonstrates
2. **Feature Sections**: Each feature has its own section with comments
3. **Demo Functions**: Individual functions demonstrating each feature
4. **Main Function**: Orchestrates all demos
5. **Real-World Example**: Practical application of the features

## Learning Path

We recommend exploring the examples in this order:

1. **hello_world.rscc** - Understand basic programme structure
2. **phase1_features.rscc** - Learn modern syntax enhancements
3. **phase2_features.rscc** - Explore advanced function capabilities
4. **phase3_features.rscc** - Master safety and metaprogramming
5. **phase4_features.rscc** - Discover 77 advanced language features
6. **hideseek.rscc** - See a complete application in action

## Compiling to WebAssembly

All examples can also be compiled to WebAssembly:

**On Windows:**
```powershell
..\target\release\rsxe.exe phase1_features.rscc --target wasm
```

**On Linux/Mac:**
```bash
../target/release/rsxe phase1_features.rscc --target wasm
```

The WASM file can be loaded in a browser.

## Running in the Browser

Create an HTML file:

```html
<!DOCTYPE html>
<html>
<head>
    <title>RustScript Example</title>
</head>
<body>
    <h1>Check the Console (F12)</h1>
    <script type="text/rustscript" src="phase1_features.rscc"></script>
</body>
</html>
```

Then serve it:

**On Windows:**
```powershell
python ..\serve.py
```

**On Linux/Mac:**
```bash
python3 ../serve.py
```

Open `http://localhost:8000` in your browser.

## Contributing Examples

Have a great example? We'd love to include it!

1. Create a new `.rscc` file
2. Add comprehensive comments explaining the code
3. Include a real-world use case
4. Test that it compiles and runs correctly
5. Submit a pull request

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

## Example Categories

### Basic Examples
- `hello_world.rscc` - Your first programme

### Feature Demonstrations
- `phase1_features.rscc` - Syntax enhancements (4 features)
- `phase2_features.rscc` - Function enhancements (3 features)
- `phase3_features.rscc` - Safety features (3 features)
- `phase4_features.rscc` - Advanced features (77 features)

### Complete Applications
- `hideseek.rscc` - Text adventure game

### Coming Soon
- Data structures and algorithms
- Web application examples
- API client examples

## Need Help?

- 📚 Read the [Tutorial](../docs/TUTORIAL.md)
- 📖 Check the [Phase Documentation](../docs/)
- 💬 Ask questions in GitHub Discussions
- 🐛 Report issues on GitHub

Happy coding with RustScript! 🦀
