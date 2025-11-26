# RustScript Examples

**Author**: Michael Lauzon

This directory contains example programmes demonstrating RustScript features.

## Getting Started

### Hello World

The simplest RustScript programme:

```bash
../target/release/rjsc hello_world.rjsc
node hello_world.js
```

## Feature Examples

### Phase 1: String & Syntax Enhancements

**File**: `phase1_features.rjsc`

Demonstrates:
- String interpolation with `{expression}` syntax
- Optional chaining with `?.` operator
- Null coalescing with `??` operator
- List comprehensions for collection transformation

**Run it:**
```bash
../target/release/rjsc phase1_features.rjsc
node phase1_features.js
```

### Phase 2: Function Enhancements

**File**: `phase2_features.rjsc`

Demonstrates:
- Pattern matching in function heads (Erlang-style)
- Generators with `yield` keyword
- Multiple dispatch (Julia-style)
- Lazy evaluation and infinite sequences

**Run it:**
```bash
../target/release/rjsc phase2_features.rjsc
node phase2_features.js
```

### Phase 3: Safety & Metaprogramming

**File**: `phase3_features.rjsc`

Demonstrates:
- Design by Contract (`requires`, `ensures`, `invariant`)
- Effect system (`effect [pure]`, `effect [io]`, etc.)
- Compile-time execution with `comptime` blocks
- Formal verification and safety guarantees

**Run it:**
```bash
../target/release/rjsc phase3_features.rjsc
node phase3_features.js
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

1. **hello_world.rjsc** - Understand basic programme structure
2. **phase1_features.rjsc** - Learn modern syntax enhancements
3. **phase2_features.rjsc** - Explore advanced function capabilities
4. **phase3_features.rjsc** - Master safety and metaprogramming

## Compiling to WebAssembly

All examples can also be compiled to WebAssembly:

```bash
# Build WASM
../target/release/rjsc phase1_features.rjsc --target wasm

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
    <script type="text/rustscript" src="phase1_features.rjsc"></script>
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

1. Create a new `.rjsc` file
2. Add comprehensive comments explaining the code
3. Include a real-world use case
4. Test that it compiles and runs correctly
5. Submit a pull request

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

## Example Categories

### Basic Examples
- `hello_world.rjsc` - Your first programme

### Feature Demonstrations
- `phase1_features.rjsc` - Syntax enhancements
- `phase2_features.rjsc` - Function enhancements
- `phase3_features.rjsc` - Safety features

### Coming Soon
- Data structures and algorithms
- Web application examples
- API client examples
- Game development examples

## Need Help?

- 📚 Read the [Tutorial](../docs/TUTORIAL.md)
- 📖 Check the [Phase Documentation](../docs/)
- 💬 Ask questions in GitHub Discussions
- 🐛 Report issues on GitHub

Happy coding with RustScript! 🦀
