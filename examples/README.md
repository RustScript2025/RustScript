# RustScript Examples

**Author**: Michael Lauzon

This directory contains example programmes demonstrating RustScript features.

## Getting Started

### Hello World

The simplest RustScript programme:

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

**Run it:**
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

**Run it:**
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

**Run it:**
```bash
../target/release/rsxe phase3_features.rscc
node phase3_features.js
```

### Phase 4: Advanced Language Features

**File**: `phase4_features.rscc`

Demonstrates 72 advanced features across 8 categories:
- **4A**: Memory safety (lifetimes, borrowing, tail call optimisation)
- **4B**: Advanced types (union types, GADTs, higher-kinded types)
- **4C**: Functional programming (partial application, currying, lenses)
- **4D**: Concurrency (async/await, channels, parallel iterators)
- **4E**: Control flow (try blocks, guard clauses, defer)
- **4F**: Metaprogramming (macros, reflection, code generation)
- **4G**: Domain-specific (regex literals, operator overloading)
- **4H**: Utilities (default parameters, const functions)

**Run it:**
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

**Run it:**
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
5. **phase4_features.rscc** - Discover 72 advanced language features
6. **hideseek.rscc** - See a complete application in action

## Compiling to WebAssembly

All examples can also be compiled to WebAssembly:

```bash
# Build WASM
../target/release/rsxe phase1_features.rscc --target wasm

# The WASM file can be loaded in a browser
```

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

```bash
python ../serve.py
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
- `phase4_features.rscc` - Advanced features (72 features)

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
