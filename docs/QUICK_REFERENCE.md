# RustScript Quick Reference

**Author**: Michael Lauzon

A concise reference for RustScript syntax and features.

## Basic Syntax

### Variables

```rustscript
let x = 5;              // Immutable
let mut y = 10;         // Mutable
const MAX = 100;        // Constant (compile-time)
```

### Functions

```rustscript
fn add(a: number, b: number) -> number {
    a + b  // Implicit return
}

// Named arguments
greet(name: "Alice", formal: true);
```

### Types

```rustscript
number      // Floating-point number
string      // UTF-8 string
bool        // Boolean (true/false)
[T]         // Array of type T
(T, U)      // Tuple
```

## Control Flow

### If Expression

```rustscript
let result = if condition {
    "yes"
} else {
    "no"
};
```

### Loops

```rustscript
loop { }                    // Infinite loop
while condition { }         // While loop
for item in collection { }  // For loop
```

### Match

```rustscript
match value {
    0 => "zero",
    1..=10 => "small",
    _ => "large"
}
```

## Data Structures

### Structs

```rustscript
struct Point {
    x: number,
    y: number
}

impl Point {
    fn new(x: number, y: number) -> Point {
        Point { x, y }
    }
}
```

### Arrays

```rustscript
let numbers = [1, 2, 3, 4, 5];
let first = numbers[0];
```

### Tuples

```rustscript
let point = (10, 20);
let (x, y) = point;  // Destructuring
```

## Error Handling

### Result Type

```rustscript
fn divide(a: number, b: number) -> Result<number, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

match divide(10.0, 2.0) {
    Ok(result) => console.log(result),
    Err(error) => console.error(error)
}
```

### Option Type

```rustscript
fn find(id: number) -> Option<string> {
    if id == 1 {
        Some("Alice")
    } else {
        None
    }
}
```

## Phase 1 Features

### String Interpolation

```rustscript
let name = "Alice";
let message = "Hello, {name}!";
let calc = "2 + 2 = {2 + 2}";
```

### Optional Chaining

```rustscript
let city = user?.address?.city;
```

### Null Coalescing

```rustscript
let name = username ?? "Guest";
let value = first() ?? second() ?? "default";
```

### List Comprehensions

```rustscript
let doubled = [x * 2 for x in numbers];
let evens = [x for x in numbers if x % 2 == 0];
let squares = [x * x for x in numbers if x > 0];
```

## Phase 2 Features

### Pattern Matching in Function Heads

```rustscript
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }

fn sum([]) -> number { 0 }
fn sum([head, ...tail]) -> number { head + sum(tail) }
```

### Generators

```rustscript
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}

for fib in take(10, fibonacci()) {
    console.log(fib);
}
```

### Multiple Dispatch

```rustscript
fn process(x: number, y: number) -> number { x + y }
fn process(x: string, y: string) -> string { x + y }
fn process(x: number, y: string) -> string { "{x}{y}" }
```

## Phase 3 Features

### Design by Contract

```rustscript
fn withdraw(balance: number, amount: number) -> number
    requires amount > 0, "Amount must be positive"
    requires balance >= amount, "Insufficient funds"
    ensures result >= 0, "Balance cannot be negative"
{
    balance - amount
}
```

### Effect System

```rustscript
effect [pure]
fn add(a: number, b: number) -> number { a + b }

effect [io]
fn log(msg: string) { console.log(msg); }

effect [io, state, throws]
fn process() { /* ... */ }
```

### Compile-time Execution

```rustscript
comptime {
    const BUFFER_SIZE = 1024;
    
    fn fibonacci(n: number) -> number {
        if n <= 1 { n }
        else { fibonacci(n - 1) + fibonacci(n - 2) }
    }
    
    const FIB_10 = fibonacci(10);
}

fn main() {
    console.log("Fib(10) = {FIB_10}");  // No runtime computation
}
```

## Common Patterns

### Early Return with Guard

```rustscript
fn process(value: number) {
    guard value > 0 else {
        console.error("Invalid value");
        return;
    }
    
    // Continue with valid value
}
```

### Defer for Cleanup

```rustscript
fn process_file(path: string) {
    let file = open(path);
    
    defer {
        close(file);  // Always runs, even on error
    }
    
    // Process file
}
```

### Pipeline Operator

```rustscript
let result = data
    |> validate
    |> transform
    |> save;
```

## Operators

### Arithmetic
```rustscript
+  -  *  /  %
```

### Comparison
```rustscript
==  !=  <  >  <=  >=
```

### Logical
```rustscript
&&  ||  !
```

### Special
```rustscript
?.   // Optional chaining
??   // Null coalescing
|>   // Pipeline
```

## Keywords

```
async       await       bool        break       comptime
const       continue    defer       effect      else
ensures     enum        extend      false       fn
for         gen         guard       if          impl
import      in          invariant   let         loop
match       mut         number      requires    return
string      struct      true        use         while
yield       _
```

## Comments

```rustscript
// Single-line comment

/*
   Multi-line comment
*/
```

## Module System

```rustscript
// Import
use "path/to/module.rscc";

// Export (implicit - all top-level items are exported)
```

## Interop with JavaScript

```rustscript
// Call JavaScript functions
console.log("Hello");
Math.sqrt(16);

// Access JavaScript objects
document.getElementById("app");
```

## Compilation

```bash
# Compile to JavaScript
rsxe input.rscc

# Compile to WebAssembly
rsxe input.rscc --target wasm

# Specify output
rsxe input.rscc --output dist/output.rscx
```

## Tips

1. **Start with immutable** - Use `let` by default, add `mut` only when needed
2. **Use pattern matching** - More expressive than if-else chains
3. **Leverage type inference** - Let the compiler figure out types
4. **Handle errors explicitly** - Use `Result` and `Option` types
5. **Document with contracts** - Use `requires`/`ensures` for important functions
6. **Think in expressions** - Most things return values in RustScript

## See Also

- [Complete Tutorial](TUTORIAL.md)
- [Phase 1 Features](PHASE1_FEATURES.md)
- [Phase 2 Features](PHASE2_FEATURES.md)
- [Phase 3 Features](PHASE3_FEATURES.md)
- [Examples](../examples/)
