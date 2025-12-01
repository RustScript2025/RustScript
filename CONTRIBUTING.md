# Contributing to RustScript

**Author**: Michael Lauzon

Thank you for your interest in contributing to RustScript! This document provides guidelines and information for contributors.

## Development Setup

### Prerequisites

- Rust 1.91.1 or later
- Cargo (comes with Rust)
- Git
- A code editor with Rust support (recommended: VS Code with rust-analyzer)

### Getting Started

1. **Fork and Clone**
   ```bash
   git clone https://github.com/YOUR_USERNAME/RustScript.git
   cd RustScript
   ```

2. **Build the Project**
   ```bash
   cargo build
   ```

3. **Run Tests**
   ```bash
   cargo test
   ```

4. **Check Your Code**
   ```bash
   cargo clippy -- -D warnings
   cargo fmt --check
   ```

## Code Style and Standards

### Rust Edition 2024

RustScript uses **Rust Edition 2024**. Please familiarise yourself with the latest features:

- **Implicit Format Arguments**: Use `format!("{name}")` instead of `format!("{}", name)`
- **Modern Error Handling**: Prefer `?` operator and `Result` types
- **Pattern Matching**: Use exhaustive `match` expressions
- **Ownership**: Follow Rust's ownership rules strictly

### Code Quality Guidelines

1. **No `unwrap()` in Production Code**
   - Use proper error handling with `Result` or `Option`
   - Only use `unwrap()` when you can prove it's safe (with a comment explaining why)
   - Prefer `unwrap_or()`, `unwrap_or_else()`, or `?` operator

2. **Comprehensive Documentation**
   - All public functions must have doc comments
   - Explain the "why", not just the "what"
   - Include examples for complex functions

3. **Error Messages**
   - Provide helpful, actionable error messages
   - Include source context when possible
   - Use Canadian English spelling (behaviour, colour, etc.)

4. **Testing**
   - Write tests for new features
   - Include both positive and negative test cases
   - Test edge cases

### Example: Good vs Bad Code

**❌ Bad:**
```rust
fn process_file(path: &str) -> String {
    let content = std::fs::read_to_string(path).unwrap();
    let result = parse(content).unwrap();
    format!("Processed: {}", result)
}
```

**✅ Good:**
```rust
fn process_file(path: &str) -> anyhow::Result<String> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {path}"))?;
    let result = parse(&content)
        .with_context(|| format!("Failed to parse file: {path}"))?;
    Ok(format!("Processed: {result}"))
}
```

## Project Structure

```
RustScript/
├── src/
│   ├── main.rs           # CLI entry point
│   ├── lib.rs            # Library and WASM interface
│   ├── ast.rs            # Abstract Syntax Tree definitions
│   ├── lexer.rs          # Tokenisation
│   ├── rustscript.pest   # Parser grammar (Pest PEG format)
│   ├── typechecker.rs    # Type inference and checking
│   ├── borrow_checker.rs # Memory safety analysis
│   ├── codegen_wasm.rs   # WebAssembly code generation
│   ├── diagnostics.rs    # Error reporting
│   ├── memory.rs         # Memory management
│   ├── sourcemap.rs      # Source map generation
│   └── std_lib.rs        # Standard library definitions
├── docs/                 # Documentation
├── examples/             # Example .rscc programmes
├── www/                  # Browser runtime
└── Cargo.toml           # Project configuration
```

## Adding New Features

### Phase System

RustScript features are organised into phases:

- **Phase 1**: Syntax enhancements (string interpolation, optional chaining, etc.)
- **Phase 2**: Function enhancements (generators, multiple dispatch, etc.)
- **Phase 3**: Safety features (contracts, effects, compile-time execution)

When adding a new feature:

1. **Update the AST** (`src/ast.rs`)
   - Add new expression or statement types
   - Include proper documentation

2. **Update the Parser** (`src/rustscript.pest`)
   - Add grammar rules for the new syntax using PEG notation
   - Ensure proper precedence

3. **Update the Type Checker** (`src/typechecker.rs`)
   - Add type checking logic
   - Handle new type inference cases

4. **Update Code Generation** (`src/codegen_wasm.rs`)
   - Implement WASM generation for the feature
   - Test the generated code

5. **Add Documentation**
   - Update relevant phase documentation
   - Add examples to the tutorial
   - Update the README if it's a major feature

6. **Write Tests**
   - Add test cases in `examples/`
   - Verify compilation and execution

## Documentation Standards

### Canadian English

RustScript uses **Canadian English** throughout:

- behaviour (not behavior)
- colour (not color)
- optimisation (not optimization)
- whilst (not while, in prose)
- analyse (not analyze)
- programme (not program, when referring to software)

### Documentation Comments

Use Rust's documentation comment syntax:

```rust
/// Checks if a type is compatible with another type.
/// 
/// This function performs structural type checking, considering
/// subtyping relationships and type inference.
/// 
/// # Arguments
/// 
/// * `expected` - The expected type
/// * `actual` - The actual type to check
/// 
/// # Returns
/// 
/// Returns `Ok(())` if types are compatible, or an error describing
/// the type mismatch.
/// 
/// # Examples
/// 
/// ```ignore
/// let result = check_type_compatibility(&Type::Number, &Type::Number);
/// assert!(result.is_ok());
/// ```
pub fn check_type_compatibility(expected: &Type, actual: &Type) -> Result<(), String> {
    // Implementation
}
```

## Commit Messages

Follow conventional commit format:

```
type(scope): brief description

Longer description if needed, explaining what and why.

Fixes #123
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

**Examples:**
```
feat(parser): add support for optional chaining operator

Implements the ?. operator for safe navigation through
potentially null values.

Closes #45
```

```
fix(typechecker): handle recursive type definitions

Previously, recursive types would cause infinite loops.
Now properly detects and handles recursive definitions.

Fixes #67
```

## Pull Request Process

1. **Create a Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make Your Changes**
   - Follow the code style guidelines
   - Add tests
   - Update documentation

3. **Test Thoroughly**
   ```bash
   cargo test
   cargo clippy
   cargo fmt
   ```

4. **Commit Your Changes**
   - Use conventional commit messages
   - Keep commits focused and atomic

5. **Push and Create PR**
   ```bash
   git push origin feature/your-feature-name
   ```
   - Create a pull request on GitHub
   - Fill out the PR template
   - Link related issues

6. **Code Review**
   - Address reviewer feedback
   - Keep the PR updated with main branch

## Testing

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_name

# Run with output
cargo test -- --nocapture
```

### Writing Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_inference() {
        let mut checker = TypeChecker::new();
        let expr = /* create test expression */;
        let result = checker.infer_type(&expr);
        assert!(result.is_ok());
    }
}
```

## Getting Help

- **Issues**: Check existing issues or create a new one
- **Discussions**: Use GitHub Discussions for questions
- **Documentation**: Read the docs in the `docs/` directory

## Code of Conduct

- Be respectful and inclusive
- Provide constructive feedback
- Focus on the code, not the person
- Help others learn and grow

## Recognition

Contributors will be recognised in:
- The project README
- Release notes
- Git commit history

Thank you for contributing to RustScript! 🦀
