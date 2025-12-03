# Phase 4 Features: Advanced Language Capabilities

**Author**: Michael Lauzon  
**Completed**: December 1, 2025

Phase 4 represents a massive expansion of RustScript's capabilities, adding 77 advanced features across 9 sub-phases. These features bring RustScript to feature parity with modern systems programming languages whilst maintaining its unique blend of Rust's safety and JavaScript's ergonomics.

## Table of Contents

1. [Overview](#overview)
2. [Phase 4A: Core Memory Safety](#phase-4a-core-memory-safety-10-features)
3. [Phase 4B: Advanced Type System](#phase-4b-advanced-type-system-15-features)
4. [Phase 4C: Functional Programming](#phase-4c-functional-programming-10-features)
5. [Phase 4D: Concurrency & Parallelism](#phase-4d-concurrency--parallelism-8-features)
6. [Phase 4E: Advanced Control Flow](#phase-4e-advanced-control-flow-10-features)
7. [Phase 4F: Metaprogramming & Macros](#phase-4f-metaprogramming--macros-8-features)
8. [Phase 4G: Domain-Specific Features](#phase-4g-domain-specific-features-6-features)
9. [Phase 4H: Additional Utilities](#phase-4h-additional-utilities-5-features)
10. [Phase 4I: MUSHcode-Inspired Features](#phase-4i-mushcode-inspired-features-5-features)
11. [Language Origins Summary](#language-origins-summary)
12. [Why Phase 4 Matters](#why-phase-4-matters)
13. [See Also](#see-also)

---

## Overview

Phase 4 includes **77 features** organised into 9 sub-phases:

- **Phase 4A**: Core Memory Safety (10 features)
- **Phase 4B**: Advanced Type System (15 features)
- **Phase 4C**: Functional Programming (10 features)
- **Phase 4D**: Concurrency & Parallelism (8 features)
- **Phase 4E**: Advanced Control Flow (10 features)
- **Phase 4F**: Metaprogramming & Macros (8 features)
- **Phase 4G**: Domain-Specific Features (6 features)
- **Phase 4H**: Additional Utilities (5 features)
- **Phase 4I**: MUSHcode-Inspired Features (5 features)

All features are **fully implemented** and available in RustScript today.

---

## Phase 4A: Core Memory Safety (10 Features)

These features provide Rust-level memory safety without garbage collection.

### 1. Lifetimes

**Inspired by**: Rust (2010), Cyclone (2002)

Lifetime annotations prevent dangling references at compile time.

```rustscript
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length > y.length { x } else { y }
}
```

**Why it matters**: Catches use-after-free bugs at compile time, not runtime.

### 2. Borrowing & References

**Inspired by**: Rust (2010)

Immutable (`&T`) and mutable (`&mut T`) references with compile-time checking.

```rustscript
fn read_balance(account: &Account) -> number {
    account.balance  // Can read but not modify
}

fn deposit(account: &mut Account, amount: number) {
    account.balance += amount  // Can modify
}
```

**Why it matters**: Multiple readers OR one writer - prevents data races.


### 3. Move Semantics

**Inspired by**: Rust (2010), C++ (2011)

Explicit ownership transfer with the `move` keyword.

```rustscript
let data = create_large_data();
let moved_data = move data;  // Ownership transferred
// data is no longer valid here
```

**Why it matters**: Zero-cost ownership transfer, no copying large data structures.

### 4. Tail Call Optimisation

**Inspired by**: Scheme (1975), Lua (1993)

Tail-recursive functions are automatically optimised to loops.

```rustscript
fn factorial(n: number, acc: number) -> number {
    if n <= 1 { acc } else { factorial(n - 1, n * acc) }
}
// No stack overflow, even with factorial(10000, 1)
```

**Why it matters**: Write recursive code without worrying about stack limits.

### 5. Pattern Guards

**Inspired by**: Haskell (1990), Erlang (1986)

Add conditions to pattern matching arms.

```rustscript
match value {
    x if x < 0 => "negative",
    x if x % 2 == 0 => "positive even",
    _ => "positive odd"
}
```

**Why it matters**: More expressive pattern matching without nested if-else.

### 6. Traits (RAII & Drop)

**Inspired by**: Rust (2010), C++ RAII (1984)

Resource Acquisition Is Initialisation with automatic cleanup.

```rustscript
trait Drop {
    fn drop(&mut self);
}

impl Drop for FileHandle {
    fn drop(&mut self) {
        close_file(self.path);  // Automatic cleanup
    }
}
```

**Why it matters**: No resource leaks, guaranteed cleanup even on errors.

### 7. Const Generics

**Inspired by**: Rust (2020), C++ templates (1990)

Compile-time constant parameters for types.

```rustscript
struct FixedArray<T, const N: usize> {
    data: [T; N]  // Array of exactly N elements
}
```

**Why it matters**: Type-safe fixed-size arrays with compile-time checking.

### 8. Algebraic Effects

**Inspired by**: Eff (2012), Koka (2012)

Composable effect handlers for side effects.

```rustscript
handle {
    perform Log("Starting operation");
    let result = compute();
    perform Log("Operation complete");
    result
} with {
    Log(msg) => {
        console.log("[LOG] {msg}");
        resume(0)
    }
}
```

**Why it matters**: Separate effect definition from handling, composable side effects.

### 9. Inline Assembly

**Inspired by**: Rust (2015), C (1972)

Direct WebAssembly instruction emission for maximum performance.

```rustscript
fn add_inline(a: number, b: number) -> number {
    asm! {
        local.get 0
        local.get 1
        f64.add
    }
}
```

**Why it matters**: Zero abstraction overhead for performance-critical code.

### 10. Complete Trait System

**Inspired by**: Rust (2010), Haskell type classes (1988)

Full trait system with bounds, associated types, and default implementations.

```rustscript
trait Drawable {
    fn draw(&self);
    fn area(&self) -> number;
}

impl Drawable for Circle {
    fn draw(&self) { /* ... */ }
    fn area(&self) -> number { 3.14159 * self.radius * self.radius }
}
```

**Why it matters**: Polymorphism without inheritance, zero-cost abstractions.

---

## Phase 4B: Advanced Type System (15 Features)

Sophisticated type system features for catching bugs at compile time.

### 11. Union & Intersection Types

**Inspired by**: TypeScript (2012), Ceylon (2011)

Combine types with `|` (union) and `&` (intersection).

```rustscript
type StringOrNumber = string | number;
type Printable & Serialisable = { /* both traits */ };
```

**Why it matters**: Flexible type combinations without complex hierarchies.

### 12. Type Aliases

**Inspired by**: Haskell (1990), Rust (2010), TypeScript (2012)

Named aliases for complex types.

```rustscript
type UserId = number;
type Result<T> = Ok(T) | Err(string);
```

**Why it matters**: Self-documenting code, easier refactoring.

### 13. Newtype Pattern

**Inspired by**: Haskell (1990), Rust (2010)

Zero-cost type-safe wrappers.

```rustscript
struct Metres(number);
struct Kilometres(number);

fn calculate_speed(distance: Metres, time: Seconds) -> number {
    distance.0 / time.0
}
```

**Why it matters**: Prevents mixing incompatible units at compile time.

### 14. Associated Types

**Inspired by**: Rust (2015), Haskell (1996)

Types associated with traits for cleaner generic code.

```rustscript
trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
```

**Why it matters**: Cleaner trait definitions, better type inference.

### 15. Higher-Kinded Types

**Inspired by**: Haskell (1990), Scala (2004)

Type constructors that take type parameters.

```rustscript
trait Functor<F<_>> {
    fn map<A, B>(fa: F<A>, f: fn(A) -> B) -> F<B>;
}
```

**Why it matters**: Abstract over type constructors, powerful generic programming.


### 16-25. Additional Type System Features

- **Phantom Types**: Zero-cost compile-time type safety markers
- **Refinement Types**: Types with predicates (`{x: i32 | x > 0}`)
- **Dependent Types**: Types that depend on runtime values
- **Type-Level Programming**: Computation at the type level
- **Existential Types**: Hide concrete types behind interfaces (`impl Trait`)
- **GADTs**: Generalised Algebraic Data Types with constructor-specific return types
- **Variance Annotations**: Covariant (`+T`), contravariant (`-T`), invariant (`T`)
- **Type Bounds**: Trait bounds and where clauses
- **Subtyping**: Type relationships and variance-based substitution
- **Structural Typing**: Duck typing based on structure rather than names

---

## Phase 4C: Functional Programming (10 Features)

Pure functional programming features for more reliable code.

### 26. Partial Application

**Inspired by**: Haskell (1990), ML (1973), F# (2005)

Fix some arguments whilst leaving others as placeholders.

```rustscript
let add5 = add(5, _);  // Fix first argument
let result = add5(10);  // result = 15
```

### 27. Function Composition

**Inspired by**: Haskell (1990), F# (2005), Elixir (2011)

Combine functions with `>>` (forward) and `<<` (backward).

```rustscript
let process = validate >> transform >> save;
let result = process(data);
```

### 28. Currying

**Inspired by**: Haskell (1990), ML (1973), OCaml (1996)

Transform multi-parameter functions into single-parameter chains.

```rustscript
fn add(a: number)(b: number) -> number { a + b }
let add5 = add(5);
let result = add5(10);  // 15
```

### 29-35. Additional Functional Features

- **Lazy Evaluation**: Defer computation until needed
- **Memoisation**: Automatic result caching with `@memoize`
- **Immutable Data Structures**: Persistent collections with structural sharing
- **Transducers**: Composable algorithmic transformations
- **Do-Notation**: Haskell-style monadic composition
- **Applicative Functors**: `<$>` and `<*>` operators
- **Lenses**: Composable getters/setters for immutable updates

---

## Phase 4D: Concurrency & Parallelism (8 Features)

Safe concurrency without data races.

### 36. Async/Await

**Inspired by**: C# (2012), JavaScript (2017), Rust (2019)

Asynchronous programming with futures.

```rustscript
async fn fetch_data(url: string) -> Result<string, Error> {
    let response = await http.get(url)?;
    Ok(response.text())
}
```

### 37. Channels

**Inspired by**: Go (2009), Rust (2015)

Message passing between concurrent tasks.

```rustscript
let (tx, rx) = channel();
spawn { tx.send(42); };
let msg = rx.recv();
```

### 38-43. Additional Concurrency Features

- **Futures & Streams**: Asynchronous iterators
- **Mutex & RwLock**: Mutual exclusion and reader-writer locks
- **Atomic Operations**: Lock-free synchronisation
- **Parallel Iterators**: Data-parallel computation
- **Scoped Threads**: Safe borrowing in spawned threads
- **Select**: Channel multiplexing

---

## Phase 4E: Advanced Control Flow (10 Features)

Explicit error handling and control flow.

### 44. Try Blocks

**Inspired by**: Rust (2018), Kotlin (2011)

Try blocks with multiple error types.

```rustscript
let result = try {
    let file = read_file("data.txt")?;
    let data = parse_data(file)?;
    Ok(data)
} catch FileError as e {
    Err(AppError::File(e))
} catch ParseError as e {
    Err(AppError::Parse(e))
};
```

### 45. Try Operator (?)

**Inspired by**: Rust (2016), Swift (2014)

Concise error propagation.

```rustscript
fn process() -> Result<Data, Error> {
    let file = read_file("data.txt")?;
    let data = parse_data(file)?;
    Ok(data)
}
```

### 46-53. Additional Control Flow Features

- **Guard Clauses**: Early returns with `guard let`
- **Labelled Blocks**: Break with values from nested blocks
- **Catch Expressions**: Inline error handling
- **Panic with Backtraces**: Enhanced panic messages
- **Defer Statements**: Guaranteed cleanup (LIFO order)
- **Conditional Compilation**: `#[cfg]` attributes
- **Const Assertions**: Compile-time validation
- **Unreachable Markers**: Optimisation hints

---

## Phase 4F: Metaprogramming & Macros (8 Features)

Compile-time code generation and transformation.

### 54. Declarative Macros

**Inspired by**: Rust macro_rules! (2015), Lisp (1958)

Pattern-based code generation.

```rustscript
macro_rules! vec {
    ($($x:expr),*) => {
        {
            let mut temp_vec = Vec::new();
            $(temp_vec.push($x);)*
            temp_vec
        }
    };
}
```

### 55. Procedural Macros

**Inspired by**: Rust (2018), Lisp (1958)

Function-like and derive macros.

```rustscript
#[derive(Debug, Clone, PartialEq)]
struct User {
    name: string,
    age: number,
}
```

### 56-61. Additional Metaprogramming Features

- **Attribute Macros**: Transform code with attributes
- **Compile-Time Reflection**: `@typeInfo` queries
- **Code Generation**: `comptime` blocks and `emit()`
- **Quasiquoting**: `quote!` templates
- **Hygiene**: Automatic variable capture prevention
- **Syntax Extensions**: Custom DSL support

---

## Phase 4G: Domain-Specific Features (6 Features)

Language-level support for common patterns.

### 62. Regex Literals

**Inspired by**: Perl (1987), JavaScript (1995), Ruby (1995)

First-class regex support.

```rustscript
let email_pattern = r/[\w\.-]+@[\w\.-]+\.\w+/;
if email_pattern.test(email) {
    console.log("Valid email");
}
```

### 63. Format Strings

**Inspired by**: Python f-strings (2015), Rust (2018)

Advanced string formatting.

```rustscript
let message = f"Hello, {name:>10}! Score: {score:6.2f}";
```

### 64-67. Additional Domain Features

- **String Slicing**: Python-style `str[start..end]`
- **Operator Overloading**: Trait-based custom operators
- **Custom Indexing**: `Index` trait for `[]` operator
- **Destructuring Assignment**: Tuple and record patterns

---

## Phase 4H: Additional Utilities (5 Features)

Small conveniences that improve developer experience.

### 68. Ranges with Step

**Inspired by**: Python (1991), Ruby (1995), Rust (2015)

Custom iteration increments.

```rustscript
for i in (0..20).step_by(5) {
    console.log(i);  // 0, 5, 10, 15
}
```

### 69-72. Additional Utilities

- **Zip Iterator**: Combine multiple iterators
- **Enumerate**: Add indices to values
- **Default Parameters**: Function parameter defaults
- **Const Functions**: Compile-time evaluation

---

## Phase 4I: MUSHcode-Inspired Features (5 Features)

Features inspired by MUSHcode/Softcode (1990), a functional scripting language created by Larry Foard for multi-user text environments.

### 73. Iteration Placeholders

**Inspired by**: MUSHcode iter() function (1990)

Concise iteration syntax where `##` represents the current element and `#@` represents the index.

```rustscript
// ## for current value, #@ for index
let doubled = [## * 2 for ## in numbers];
let indexed = ["{#@}: {##}" for ## in items];

// In iter expressions
iter(numbers, ## * 2)
iter(items, "{#@}. {##}")
```

**Why it matters**: More concise than naming iteration variables, reduces variable name pollution.

### 74. Register Variables

**Inspired by**: MUSHcode setq()/setr() registers (1990)

Fast temporary storage using numbered registers `%q0` through `%q9`, scoped to the current function.

```rustscript
fn calculate_stats(data: [number]) -> (number, number, number) {
    %q0 = 0.0;  // sum
    %q1 = data[0];  // min
    %q2 = data[0];  // max
    
    for value in data {
        %q0 = %q0 + value;
        if value < %q1 { %q1 = value; }
        if value > %q2 { %q2 = value; }
    }
    
    (%q0 / data.length, %q1, %q2)  // (avg, min, max)
}
```

**Why it matters**: Faster than hash-based variable lookup, useful for accumulator patterns.

### 75. String Registers

**Inspired by**: MUSHcode string accumulation patterns (1990)

String registers `%r0` through `%r9` are optimised for string building and accumulation with efficient append operations.

```rustscript
fn build_html(items: [Item]) -> string {
    %r0 = "<ul>\n";
    
    for item in items {
        %r0 .= "  <li>{item.name}</li>\n";
    }
    
    %r0 .= "</ul>";
    %r0
}
```

**Why it matters**: Efficient string building without intermediate allocations.

### 76. Literal Operator

**Inspired by**: MUSHcode lit() function (1990)

The `lit!()` macro prevents evaluation of its contents, returning the literal string representation.

```rustscript
// Debugging - see the expression, not the result
let x = 5;
let y = 10;
console.log(lit!(x + y));  // Prints: "x + y"
console.log(x + y);        // Prints: 15

// Template storage
let email_template = lit!(
    Dear {recipient},
    Thank you for your order #{order_id}.
    Best regards,
    {sender}
);
```

**Why it matters**: Enables code-as-data patterns, useful for templating and metaprogramming.

### 77. Default Function

**Inspired by**: MUSHcode default()/edefault() functions (1990)

Flexible fallback values with lazy evaluation, handling empty strings and arrays (not just null).

```rustscript
// Handle empty strings (not just null)
let name = default(user_input, "Anonymous");  // "" -> "Anonymous"

// With null coalescing comparison
let a = "" ?? "fallback";      // Returns "" (not null)
let b = default("", "fallback"); // Returns "fallback" (empty string)

// Custom predicate
let positive = default(value, 1, |x| x > 0);  // Use 1 if value <= 0

// Chained defaults
let config = default(
    env.get("CONFIG"),
    default(file.read("config.json"), default_config())
);
```

**Why it matters**: More flexible than `??`, supports predicates and handles empty values.

---

## Language Origins Summary

Phase 4 features draw from over 30 languages:

**Primary Inspirations**:
- **Rust** (2010): Memory safety, traits, lifetimes, borrowing, async/await
- **Haskell** (1990): Type system, functional programming, type classes
- **TypeScript** (2012): Advanced types, structural typing
- **C++** (1983-2011): RAII, move semantics, operator overloading
- **Lisp** (1958): Macros, metaprogramming
- **Go** (2009): Channels, defer, select
- **Zig** (2016): Compile-time execution, comptime
- **Python** (1991): String slicing, utilities
- **Scala** (2004): Higher-kinded types, variance
- **Kotlin** (2011): Try blocks, catch expressions

**Additional Influences**: Scheme, ML, OCaml, F#, Erlang, Elixir, Swift, C#, JavaScript, Clojure, Koka, Eff, Cyclone, Ceylon, Idris, Agda, Coq, Liquid Haskell, F*, Perl, Ruby, Java, MUSHcode (1990), and more.

---

## Why Phase 4 Matters

Phase 4 transforms RustScript from a modern scripting language into a full-featured systems programming language:

1. **Memory Safety**: Rust-level safety without garbage collection
2. **Type Safety**: Sophisticated type system catches bugs at compile time
3. **Zero-Cost Abstractions**: High-level features with no runtime overhead
4. **Safe Concurrency**: Data race freedom guaranteed by the type system
5. **Metaprogramming**: Compile-time code generation for maximum flexibility
6. **Developer Experience**: Modern conveniences without sacrificing safety

With 87 total features (10 from Phases 1-3, 77 from Phase 4), RustScript offers a unique combination of safety, performance, and productivity.

---

## See Also

- [Phase 1 Features](PHASE1_FEATURES.md) - String & syntax enhancements
- [Phase 2 Features](PHASE2_FEATURES.md) - Function enhancements
- [Phase 3 Features](PHASE3_FEATURES.md) - Safety & metaprogramming
- [Tutorial](TUTORIAL.md) - Complete guide with examples
- [Quick Reference](QUICK_REFERENCE.md) - Concise syntax reference
- [Examples Directory](../examples/) - Working code examples

