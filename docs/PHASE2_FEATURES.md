# Phase 2 Features: Function Enhancements

**Author**: Michael Lauzon

This document describes the Phase 2 features added to RustScript, focusing on advanced function capabilities that enable more expressive and efficient code.

## Table of Contents

1. [Overview](#overview)
2. [Feature 1: Pattern Matching in Function Heads](#feature-1-pattern-matching-in-function-heads)
3. [Feature 2: Generators](#feature-2-generators)
4. [Feature 3: Multiple Dispatch](#feature-3-multiple-dispatch)
5. [See Also](#see-also)

---

## Overview

Phase 2 introduces three major features that transform how functions work in RustScript:

1. **Pattern Matching in Function Heads** - Erlang-style function dispatch
2. **Generators** - Lazy evaluation with `yield`
3. **Multiple Dispatch** - Julia-style type-based function selection

## Feature 1: Pattern Matching in Function Heads

### Syntax

```rust
fn factorial(0) -> number {
    1
}

fn factorial(n) -> number if n > 0 {
    n * factorial(n - 1)
}
```

### Description

Pattern matching in function heads allows you to define multiple versions of the same function, each handling different input patterns. The runtime automatically selects the appropriate version based on the arguments.

### Benefits

- **Clarity**: Each case is self-contained
- **Safety**: Exhaustive pattern matching catches edge cases
- **Elegance**: Recursive algorithms are natural to express

### Examples

#### Factorial

```rust
fn factorial(0) -> number {
    1
}

fn factorial(n) -> number if n > 0 {
    n * factorial(n - 1)
}

// Usage
let result = factorial(5);  // Returns 120
```

#### List Operations

```rust
fn sum([]) -> number {
    0
}

fn sum([head, ...tail]) -> number {
    head + sum(tail)
}

// Usage
let total = sum([1, 2, 3, 4, 5]);  // Returns 15
```

#### Option Handling

```rust
fn unwrap_or(Some(value), _default) -> any {
    value
}

fn unwrap_or(None, default) -> any {
    default
}

// Usage
let value = unwrap_or(Some(42), 0);  // Returns 42
let value = unwrap_or(None, 0);      // Returns 0
```

### Guard Clauses

Guards provide additional conditions for pattern matching:

```rust
fn classify(n) -> string if n < 0 {
    "negative"
}

fn classify(n) -> string if n == 0 {
    "zero"
}

fn classify(n) -> string if n > 0 {
    "positive"
}
```

### Pattern Types

1. **Literal Patterns**: Match specific values
   ```rust
   fn handle(0) { ... }
   fn handle(1) { ... }
   ```

2. **Variable Patterns**: Bind to any value
   ```rust
   fn handle(n) { ... }
   ```

3. **Destructuring Patterns**: Extract structure
   ```rust
   fn handle([first, second, ...rest]) { ... }
   fn handle({ x, y }) { ... }
   ```

4. **Wildcard Patterns**: Ignore values
   ```rust
   fn handle(_) { ... }
   ```

## Feature 2: Generators

### Syntax

```rust
gen fn count_up_to(max: number) {
    let i = 0;
    while i < max {
        yield i;
        i += 1;
    }
}
```

### Description

Generators are functions that can pause execution and resume later, producing a sequence of values over time. They enable lazy evaluation and efficient memory usage for large or infinite sequences.

### Benefits

- **Memory Efficient**: Values computed on demand
- **Composable**: Generators can be chained
- **Infinite Sequences**: Represent unbounded data
- **Clean Syntax**: Natural expression of iteration

### Examples

#### Basic Generator

```rust
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}

// Usage
for fib in take(10, fibonacci()) {
    console.log(fib);
}
```

#### Filtering Generator

```rust
gen fn filter_evens(numbers: [number]) {
    for n in numbers {
        if n % 2 == 0 {
            yield n;
        }
    }
}

// Usage
let evens = filter_evens([1, 2, 3, 4, 5, 6]);
for even in evens {
    console.log(even);  // Prints 2, 4, 6
}
```

#### Generator Composition

```rust
gen fn map(items: [], f: fn) {
    for item in items {
        yield f(item);
    }
}

gen fn filter(items: [], predicate: fn) {
    for item in items {
        if predicate(item) {
            yield item;
        }
    }
}

// Usage
let numbers = [1, 2, 3, 4, 5];
let squared_evens = map(
    filter(numbers, |x| x % 2 == 0),
    |x| x * x
);
```

### Generator Protocol

Generators implement the Iterator protocol:

```rust
trait Iterator<T> {
    fn next() -> Option<T>;
}
```

### Yield Semantics

- `yield value` - Produces a value and pauses
- `yield` - Produces void and pauses
- Generator state is preserved between yields
- Generators are lazy (values computed on demand)

## Feature 3: Multiple Dispatch

### Syntax

```rust
fn process(x: number, y: number) -> string {
    "Adding numbers: {x + y}"
}

fn process(x: string, y: string) -> string {
    "Concatenating: {x}{y}"
}
```

### Description

Multiple dispatch selects which function to call based on the types of ALL arguments, not just the first one (as in traditional object-oriented languages). This enables more flexible and expressive APIs.

### Benefits

- **Symmetry**: All arguments treated equally
- **Extensibility**: Add new types without modifying existing code
- **Natural**: Mathematical operations work as expected
- **Performance**: Dispatch resolved at compile time when possible

### Examples

#### Type-Based Dispatch

```rust
fn combine(x: number, y: number) -> number {
    x + y
}

fn combine(x: string, y: string) -> string {
    x + y
}

fn combine(x: number, y: string) -> string {
    "{x}{y}"
}

// Usage
let sum = combine(1, 2);              // Returns 3
let concat = combine("Hello", "World"); // Returns "HelloWorld"
let mixed = combine(42, "answer");     // Returns "42answer"
```

#### Matrix Operations

```rust
struct Matrix { ... }
struct Vector { ... }

// Matrix * Matrix
fn multiply(a: Matrix, b: Matrix) -> Matrix {
    // Matrix multiplication
}

// Matrix * Vector
fn multiply(m: Matrix, v: Vector) -> Vector {
    // Matrix-vector multiplication
}

// Vector * Vector (dot product)
fn multiply(a: Vector, b: Vector) -> number {
    // Dot product
}

// Scalar * Matrix
fn multiply(scalar: number, m: Matrix) -> Matrix {
    // Scalar multiplication
}
```

#### Distance Calculations

```rust
struct Point2D { x: number, y: number }
struct Point3D { x: number, y: number, z: number }

fn distance(p1: Point2D, p2: Point2D) -> number {
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;
    Math.sqrt(dx * dx + dy * dy)
}

fn distance(p1: Point3D, p2: Point3D) -> number {
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;
    let dz = p2.z - p1.z;
    Math.sqrt(dx * dx + dy * dy + dz * dz)
}
```

### Dispatch Resolution

1. **Exact Match**: Prefer exact type matches
2. **Specificity**: More specific types win
3. **Ambiguity**: Compile error if ambiguous
4. **Fallback**: Generic versions as fallback

### Comparison with Single Dispatch

```rust
// Single dispatch (OOP style)
class Number {
    fn add(other: Number) { ... }
}

// Multiple dispatch (RustScript)
fn add(a: Number, b: Number) { ... }
fn add(a: Number, b: String) { ... }
fn add(a: String, b: Number) { ... }
```

## Combined Usage Examples

### Example 1: Lazy Data Processing

```rust
// Pattern-matched generator
gen fn range(start: number, end: number) if start < end {
    let current = start;
    while current < end {
        yield current;
        current += 1;
    }
}

gen fn range(start: number, end: number, step: number) {
    let current = start;
    while current < end {
        yield current;
        current += step;
    }
}

// Multiple dispatch for transformation
fn transform(x: number) -> number {
    x * x
}

fn transform(x: string) -> string {
    x.uppercase()
}

// Usage
for i in range(0, 10, 2) {
    console.log(transform(i));
}
```

### Example 2: Stream Processing

```rust
struct DataPoint {
    value: number,
    category: string,
}

// Pattern-matched validation
fn validate(point: DataPoint) -> bool if point.value >= 0 {
    true
}

fn validate(_point: DataPoint) -> bool {
    false
}

// Generator for streaming
gen fn stream_valid(data: [DataPoint]) {
    for point in data {
        if validate(point) {
            yield point;
        }
    }
}

// Multiple dispatch for aggregation
fn aggregate(points: [DataPoint], method: "sum") -> number {
    let total = 0.0;
    for point in points {
        total += point.value;
    }
    total
}

fn aggregate(points: [DataPoint], method: "average") -> number {
    let sum = aggregate(points, "sum");
    sum / points.length
}
```

## Performance Considerations

### Pattern Matching

- **Compile-Time**: Patterns resolved at compile time when possible
- **Runtime**: Efficient dispatch table for dynamic cases
- **Optimisation**: Dead code elimination for unused patterns

### Generators

- **Memory**: O(1) memory for generator state
- **Lazy**: Values computed only when needed
- **Zero-Cost**: Comparable to hand-written iterators

### Multiple Dispatch

- **Static**: Type-based dispatch resolved at compile time
- **Cache**: Method cache for dynamic dispatch
- **Inline**: Small functions inlined aggressively

## Best Practices

### Pattern Matching

✅ **Do:**
```rust
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }
```

❌ **Don't:**
```rust
fn factorial(n) -> number {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

### Generators

✅ **Do:**
```rust
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}
```

❌ **Don't:**
```rust
fn fibonacci(n: number) -> [number] {
    let result = [];
    let (a, b) = (0, 1);
    for _ in 0..n {
        result.push(a);
        (a, b) = (b, a + b);
    }
    result
}
```

### Multiple Dispatch

✅ **Do:**
```rust
fn add(a: number, b: number) -> number { a + b }
fn add(a: string, b: string) -> string { a + b }
```

❌ **Don't:**
```rust
fn add(a: any, b: any) -> any {
    if typeof(a) == "number" && typeof(b) == "number" {
        a + b
    } else if typeof(a) == "string" && typeof(b) == "string" {
        a + b
    }
}
```

## Migration Guide

### From If-Else to Pattern Matching

```rust
// Before
fn factorial(n: number) -> number {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// After
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }
```

### From Arrays to Generators

```rust
// Before
fn range(start: number, end: number) -> [number] {
    let result = [];
    for i in start..end {
        result.push(i);
    }
    result
}

// After
gen fn range(start: number, end: number) {
    let i = start;
    while i < end {
        yield i;
        i += 1;
    }
}
```

### From Type Checking to Multiple Dispatch

```rust
// Before
fn process(x: any, y: any) -> any {
    if typeof(x) == "number" && typeof(y) == "number" {
        x + y
    } else if typeof(x) == "string" && typeof(y) == "string" {
        x + y
    }
}

// After
fn process(x: number, y: number) -> number { x + y }
fn process(x: string, y: string) -> string { x + y }
```

## See Also

- [Phase 1 Features](PHASE1_FEATURES.md) - String & syntax enhancements
- [Phase 3 Features](PHASE3_FEATURES.md) - Safety & contracts
- [Language Reference](LANGUAGE_REFERENCE.md) - Complete syntax guide
