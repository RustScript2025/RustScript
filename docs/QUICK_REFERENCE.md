# RustScript Quick Reference

**Author**: Michael Lauzon

A concise, scannable reference for RustScript syntax and features. For detailed explanations, see [TUTORIAL.md](TUTORIAL.md).

## Basic Syntax

### Variables

```rustscript
let x = 5;              // Immutable (default, preferred)
let mut y = 10;         // Mutable (use when needed)
const MAX = 100;        // Constant (compile-time, must be literal)

// Type annotations (optional, inferred by default)
let age: number = 30;
let name: string = "Alice";
```

### Functions

```rustscript
// Basic function
fn add(a: number, b: number) -> number {
    a + b  // Implicit return (no semicolon)
}

// Explicit return (for early exit)
fn divide(a: number, b: number) -> number {
    if b == 0.0 { return 0.0; }
    a / b
}

// Named arguments (any order)
greet(name: "Alice", formal: true);
greet(formal: false, name: "Bob");

// No return value
fn log(msg: string) {
    console.log(msg);
}
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
// Basic match
match value {
    0 => "zero",
    1..=10 => "small",
    _ => "large"
}

// With guards
match num {
    x if x < 0 => "negative",
    0 => "zero",
    x if x % 2 == 0 => "positive even",
    _ => "positive odd"
}

// Destructuring
match point {
    (0, 0) => "origin",
    (x, 0) => "on x-axis",
    (0, y) => "on y-axis",
    (x, y) => "point at ({x}, {y})"
}
```

## Data Structures

### Structs

```rustscript
// Define struct
struct Point {
    x: number,
    y: number
}

// Create instance
let p = Point { x: 10, y: 20 };

// Add methods
impl Point {
    fn new(x: number, y: number) -> Point {
        Point { x, y }  // Shorthand when names match
    }
    
    fn distance(&self) -> number {
        Math.sqrt(self.x * self.x + self.y * self.y)
    }
}

// Use methods
let p = Point::new(3, 4);
console.log(p.distance());  // 5
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
// Return Result for operations that can fail
fn divide(a: number, b: number) -> Result<number, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

// Handle with match
match divide(10.0, 2.0) {
    Ok(result) => console.log(result),
    Err(error) => console.error(error)
}

// Or use ? operator to propagate errors
fn calculate() -> Result<number, string> {
    let x = divide(10.0, 2.0)?;  // Returns early if Err
    let y = divide(x, 3.0)?;
    Ok(y)
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
let age = 30;

// Embed variables
let message = "Hello, {name}!";

// Embed expressions
let calc = "2 + 2 = {2 + 2}";
let info = "{name} is {age} years old";

// Complex expressions
let price = 100.0;
let tax = 0.13;
let total = "Total: ${price * (1.0 + tax)}";
```

### Optional Chaining

```rustscript
// Safe navigation through potentially null values
let city = user?.address?.city;

// Returns None if any part is null
let phone = user?.contact?.phone?.number;

// Combine with null coalescing
let display = user?.name ?? "Anonymous";
```

### Null Coalescing

```rustscript
// Provide default if null/None
let name = username ?? "Guest";

// Chain multiple fallbacks
let value = first() ?? second() ?? "default";

// With optional chaining
let city = user?.address?.city ?? "Unknown";
```

### List Comprehensions

```rustscript
// Transform all elements
let doubled = [x * 2 for x in numbers];

// Filter elements
let evens = [x for x in numbers if x % 2 == 0];

// Transform + filter
let squares = [x * x for x in numbers if x > 0];

// Nested (cartesian product)
let pairs = [(x, y) for x in [1,2,3] for y in [4,5,6]];
```

## Phase 2 Features

### Pattern Matching in Function Heads

```rustscript
// Match literal values
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }

// Match array patterns
fn sum([]) -> number { 0 }
fn sum([head, ...tail]) -> number { head + sum(tail) }

// Match with guards
fn classify(n) -> string if n < 0 { "negative" }
fn classify(n) -> string if n == 0 { "zero" }
fn classify(n) -> string { "positive" }
```

### Generators

```rustscript
// Define generator with 'gen fn'
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;  // Produce value lazily
        (a, b) = (b, a + b);
    }
}

// Consume with for loop
for fib in take(10, fibonacci()) {
    console.log(fib);
}

// Generator with filter
gen fn evens(numbers: [number]) {
    for n in numbers {
        if n % 2 == 0 { yield n; }
    }
}
```

### Multiple Dispatch

```rustscript
// Different implementations based on ALL argument types
fn process(x: number, y: number) -> number { x + y }
fn process(x: string, y: string) -> string { x + y }
fn process(x: number, y: string) -> string { "{x}{y}" }

// Runtime selects correct version
process(5, 10);          // -> 15 (number version)
process("Hi", "there");  // -> "Hithere" (string version)
process(42, "answer");   // -> "42answer" (mixed version)
```

## Phase 3 Features

### Design by Contract

```rustscript
// Preconditions (requires), postconditions (ensures)
fn withdraw(balance: number, amount: number) -> number
    requires amount > 0, "Amount must be positive"
    requires balance >= amount, "Insufficient funds"
    ensures result >= 0, "Balance cannot be negative"
    ensures result == balance - amount, "Correct math"
{
    balance - amount
}

// Invariants for structs
struct Account {
    balance: number
}

impl Account {
    invariant self.balance >= 0, "Balance never negative"
}
```

### Effect System

```rustscript
// Track side effects in function signatures
effect [pure]
fn add(a: number, b: number) -> number { a + b }

effect [io]
fn log(msg: string) { console.log(msg); }

effect [io, state, throws]
fn process() { /* ... */ }

// Common effects:
// pure - no side effects
// io - I/O operations
// state - modifies state
// throws - may throw errors
// alloc - allocates memory
```

### Compile-time Execution

```rustscript
// Run code at compile time
comptime {
    const BUFFER_SIZE = 1024;
    
    fn fibonacci(n: number) -> number {
        if n <= 1 { n }
        else { fibonacci(n - 1) + fibonacci(n - 2) }
    }
    
    const FIB_10 = fibonacci(10);  // Computed during compilation
}

fn main() {
    console.log("Fib(10) = {FIB_10}");  // No runtime cost!
}

// Generate code at compile time
comptime {
    for i in 0..10 {
        emit(quote! { const VAL_{i} = {i * i}; });
    }
}
```

## Common Patterns

### Early Return with Guard

```rustscript
fn process(value: number) -> Result<(), Error> {
    guard value > 0 else {
        return Err(Error::InvalidValue);
    }
    
    guard value < 100 else {
        return Err(Error::TooLarge);
    }
    
    // Happy path - no nesting!
    Ok(())
}
```

### Defer for Cleanup

```rustscript
fn process_file(path: string) -> Result<(), Error> {
    let file = open(path)?;
    
    defer {
        close(file);  // Always runs, even on error/early return
    }
    
    let data = read(file)?;
    process(data)?;
    Ok(())
}
```

### Pipeline Operator

```rustscript
// Chain operations left-to-right
let result = data
    |> validate
    |> transform
    |> save;

// Equivalent to: save(transform(validate(data)))
```

### Builder Pattern

```rustscript
let user = User::new()
    .with_name("Alice")
    .with_email("alice@example.com")
    .with_age(30)
    .build();
```

## Operators

### Arithmetic
```rustscript
+  -  *  /  %       // Add, subtract, multiply, divide, modulo
+=  -=  *=  /=      // Compound assignment
```

### Comparison
```rustscript
==  !=              // Equal, not equal
<  >  <=  >=        // Less than, greater than, etc.
```

### Logical
```rustscript
&&  ||  !           // And, or, not
```

### Special
```rustscript
?.                  // Optional chaining
??                  // Null coalescing
|>                  // Pipeline (forward)
>>  <<              // Function composition
..  ..=             // Range (exclusive, inclusive)
&  &mut             // Borrow (immutable, mutable)
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
# Compile to JavaScript (default)
rsxe input.rscc

# Compile to WebAssembly
rsxe input.rscc --target wasm

# Specify output file
rsxe input.rscc --output dist/output.js

# Watch mode (recompile on changes)
rsxe input.rscc --watch

# Development server (see SERVE.md for details)
python serve.py
```

## Best Practices

1. **Start with immutable** - Use `let` by default, add `mut` only when needed
2. **Use pattern matching** - More expressive than if-else chains
3. **Leverage type inference** - Let the compiler figure out types when obvious
4. **Handle errors explicitly** - Use `Result` and `Option` types, not exceptions
5. **Document with contracts** - Use `requires`/`ensures` for important functions
6. **Think in expressions** - Most things return values in RustScript
7. **Prefer composition** - Build complex functions from simple ones
8. **Use generators for lazy data** - Don't load everything into memory
9. **Leverage compile-time execution** - Move work from runtime to compile time
10. **Make illegal states unrepresentable** - Use types to prevent bugs

## Common Gotchas

```rustscript
// ❌ Semicolon prevents implicit return
fn add(a: number, b: number) -> number {
    a + b;  // Returns nothing! Remove semicolon
}

// ✅ Correct - no semicolon on last expression
fn add(a: number, b: number) -> number {
    a + b
}

// ❌ Forgot to call function
let result = expensive_computation;  // Just a reference

// ✅ Correct - call with ()
let result = expensive_computation();

// ❌ Trying to mutate immutable variable
let x = 5;
x = 10;  // Error!

// ✅ Declare as mutable
let mut x = 5;
x = 10;  // OK
```

## Coming from Other Languages?

### JavaScript/TypeScript → RustScript
```rustscript
// JS: var/let/const → RustScript: let/let mut/const
let x = 5;              // Like JS const
let mut y = 10;         // Like JS let
const MAX = 100;        // Like JS const (but compile-time)

// JS: template literals → RustScript: string interpolation
let msg = "Hello, {name}!";  // Same as JS `Hello, ${name}!`

// JS: optional chaining → RustScript: same!
let city = user?.address?.city;

// JS: nullish coalescing → RustScript: same!
let name = username ?? "Guest";

// JS: async/await → RustScript: same!
async fn fetch() { await http.get(url) }
```

### Python → RustScript
```rustscript
// Python: def → RustScript: fn
fn greet(name: string) { console.log("Hello, {name}!"); }

// Python: list comprehension → RustScript: same!
let squares = [x * x for x in numbers if x > 0];

// Python: generators → RustScript: same!
gen fn count() { for i in 0..10 { yield i; } }

// Python: type hints → RustScript: required for function params
fn add(a: number, b: number) -> number { a + b }
```

### Rust → RustScript
```rustscript
// Rust: i32, f64, String → RustScript: number, string
let x: number = 42;     // Rust: let x: i32 = 42;
let s: string = "hi";   // Rust: let s: String = "hi".to_string();

// Rust: println! macro → RustScript: console.log
console.log("Hello");   // Rust: println!("Hello");

// Rust: Result, Option → RustScript: same!
fn divide(a: number, b: number) -> Result<number, string> { ... }

// Rust: borrowing → RustScript: same!
fn read(data: &string) { }
fn write(data: &mut string) { }
```

### Go → RustScript
```rustscript
// Go: func → RustScript: fn
fn add(a: number, b: number) -> number { a + b }

// Go: defer → RustScript: same!
defer { cleanup(); }

// Go: channels → RustScript: same!
let (tx, rx) = channel();
tx.send(value);
let msg = rx.recv();

// Go: goroutines → RustScript: spawn
spawn { do_work(); }
```

## Syntax Cheat Sheet

| Feature | Syntax | Example |
|---------|--------|---------|
| Variable (immutable) | `let x = value;` | `let name = "Alice";` |
| Variable (mutable) | `let mut x = value;` | `let mut count = 0;` |
| Constant | `const X = value;` | `const MAX = 100;` |
| Function | `fn name(params) -> type { }` | `fn add(a: number, b: number) -> number { a + b }` |
| String interpolation | `"{expr}"` | `"Hello, {name}!"` |
| Optional chaining | `obj?.prop?.method()` | `user?.address?.city` |
| Null coalescing | `value ?? default` | `name ?? "Guest"` |
| List comprehension | `[expr for x in list if cond]` | `[x * 2 for x in nums if x > 0]` |
| Match expression | `match x { pat => val }` | `match age { 0..=17 => "minor", _ => "adult" }` |
| Result type | `Result<T, E>` | `Result<number, string>` |
| Option type | `Option<T>` | `Option<User>` |
| Error propagation | `expr?` | `let data = read_file()?;` |
| Generator | `gen fn name() { yield x; }` | `gen fn count() { for i in 0..10 { yield i; } }` |
| Async function | `async fn name() { await expr }` | `async fn fetch() { await http.get(url) }` |
| Borrow (immutable) | `&value` | `fn read(data: &string) { }` |
| Borrow (mutable) | `&mut value` | `fn write(data: &mut string) { }` |
| Defer | `defer { code }` | `defer { close(file); }` |
| Guard clause | `guard cond else { ret }` | `guard x > 0 else { return; }` |
| Pipeline | `value \|> fn1 \|> fn2` | `data \|> validate \|> save` |
| Range (exclusive) | `start..end` | `0..10` |
| Range (inclusive) | `start..=end` | `0..=10` |

## Quick Examples

### Hello World
```rustscript
fn main() {
    console.log("Hello, World!");
}
main();
```

### HTTP Request (Async)
```rustscript
async fn fetch_user(id: number) -> Result<User, Error> {
    let response = await http.get("https://api.example.com/users/{id}")?;
    Ok(response.json())
}
```

### Data Processing Pipeline
```rustscript
let results = data
    |> [x for x in _ if x.valid]           // Filter
    |> [transform(x) for x in _]           // Transform
    |> [x for x in _ if x.score > 0.8]     // Filter again
    |> sort_by(_, |x| x.score);            // Sort
```

### Type-Safe Domain Model
```rustscript
struct UserId(number);
struct Email(string);

struct User {
    id: UserId,
    email: Email,
    name: string,
}

fn send_email(to: Email, subject: string, body: string) {
    // Can't accidentally pass UserId here - compiler prevents it!
}
```

### Safe Resource Management
```rustscript
fn process_file(path: string) -> Result<(), Error> {
    let file = open(path)?;
    defer { close(file); }  // Always runs
    
    let data = read(file)?;
    process(data)?;
    Ok(())
}
```

## See Also

- **[Complete Tutorial](TUTORIAL.md)** - In-depth guide with explanations
- **[Phase 4 Features](PHASE4_FEATURES.md)** - All 72 advanced features documented
- **[Examples](../examples/)** - Working code examples
- **[Development Server](SERVE.md)** - Hot reload, HTTPS, and more


## Phase 4 Features

### Phase 4A: Memory Safety

```rustscript
// Lifetimes
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string { ... }

// Borrowing
fn read(data: &T) { }      // Immutable borrow
fn write(data: &mut T) { } // Mutable borrow

// Move semantics
let moved = move data;

// Tail call optimisation (automatic)
fn factorial(n: number, acc: number) -> number {
    if n <= 1 { acc } else { factorial(n - 1, n * acc) }
}
```

### Phase 4B: Advanced Types

```rustscript
// Union types
type StringOrNumber = string | number;

// Intersection types
type Combined = TraitA & TraitB;

// Newtype pattern
struct Metres(number);

// GADTs
enum Expr<T> {
    IntLit(i32) -> Expr<i32>,
    BoolLit(bool) -> Expr<bool>,
}

// Refinement types
type Positive = {x: i32 | x > 0};
```

### Phase 4C: Functional Programming

```rustscript
// Partial application
let add5 = add(5, _);

// Function composition
let process = f >> g >> h;

// Currying
fn add(a: number)(b: number) -> number { a + b }

// Lazy evaluation
lazy let expensive = { compute() };
let result = force(expensive);

// Memoisation
@memoize
fn fibonacci(n: number) -> number { ... }
```

### Phase 4D: Concurrency

```rustscript
// Async/await
async fn fetch(url: string) -> Result<Data, Error> {
    let response = await http.get(url)?;
    Ok(response.json())
}

// Channels
let (tx, rx) = channel();
tx.send(value);
let msg = rx.recv();

// Atomic operations
let counter = Atomic::new(0);
atomic::fetch_add(counter, 1, SeqCst);

// Parallel iterators
numbers.par_iter().map(|x| x * 2).collect()
```

### Phase 4E: Control Flow

```rustscript
// Try blocks
let result = try {
    let data = read_file()?;
    Ok(process(data))
} catch Error as e {
    Err(e)
};

// Guard clauses
guard condition else { return; };

// Labelled blocks
let result = 'outer: {
    for i in 0..10 {
        if i == 5 { break 'outer i; }
    }
    0
};

// Defer
defer { cleanup(); };
```

### Phase 4F: Metaprogramming

```rustscript
// Declarative macros
macro_rules! vec {
    ($($x:expr),*) => { ... };
}

// Derive macros
#[derive(Debug, Clone)]
struct Point { x: number, y: number }

// Compile-time reflection
comptime {
    let info = @typeInfo(MyStruct);
}

// Code generation
comptime {
    emit(quote! { fn generated() { } });
}
```

### Phase 4G: Domain-Specific

```rustscript
// Regex literals
let pattern = r/\d{3}-\d{3}-\d{4}/g;

// Format strings
let msg = f"Hello, {name:>10}! Score: {score:6.2f}";

// String slicing
let sub = text[0..5];

// Operator overloading
impl Add for Vec2 {
    fn add(self, other: Vec2) -> Vec2 { ... }
}

// Destructuring
let (x, y) = point;
let Point { x, y } = point;
```

### Phase 4H: Utilities

```rustscript
// Ranges with step
for i in (0..100).step_by(5) { }

// Zip iterator
for (a, b) in zip(list1, list2) { }

// Enumerate
for (i, item) in enumerate(items) { }

// Default parameters
fn greet(name: string = "World") { }

// Const functions
const fn add(a: number, b: number) -> number { a + b }
```

## Complete Feature List

RustScript includes **72 advanced features**:

**Phase 1 (4)**: String interpolation, Optional chaining, Null coalescing, List comprehensions  
**Phase 2 (3)**: Pattern matching in function heads, Generators, Multiple dispatch  
**Phase 3 (3)**: Design by Contract, Effect system, Compile-time execution  
**Phase 4A (10)**: Lifetimes, Borrowing, Move semantics, Tail call optimisation, Pattern guards, Traits, Const generics, Algebraic effects, Inline assembly, Complete trait system  
**Phase 4B (15)**: Union/intersection types, Type aliases, Newtype, Associated types, Higher-kinded types, Phantom types, Refinement types, Dependent types, Type-level programming, Existential types, GADTs, Variance, Type bounds, Subtyping, Structural typing  
**Phase 4C (10)**: Partial application, Composition, Currying, Lazy evaluation, Memoisation, Immutable data structures, Transducers, Do-notation, Applicative functors, Lenses  
**Phase 4D (8)**: Async/await, Channels, Futures & streams, Mutex & RwLock, Atomic operations, Parallel iterators, Scoped threads, Select  
**Phase 4E (10)**: Try blocks, Try operator, Guard clauses, Labelled blocks, Catch expressions, Panic, Defer, Conditional compilation, Const assertions, Unreachable  
**Phase 4F (8)**: Declarative macros, Procedural macros, Attribute macros, Reflection, Code generation, Quasiquoting, Hygiene, Syntax extensions  
**Phase 4G (6)**: Regex literals, Format strings, String slicing, Operator overloading, Custom indexing, Destructuring  
**Phase 4H (5)**: Ranges with step, Zip, Enumerate, Default parameters, Const functions

## Language Inspirations

RustScript draws from **60+ languages** spanning **68 years** (1958-2025):

**Primary**: Rust, Haskell, TypeScript, Python, JavaScript, C++, Lisp, Go, Zig  
**Additional**: ML, OCaml, F#, Scala, Erlang, Elixir, Swift, Kotlin, C#, Ruby, Perl, Scheme, Clojure, Julia, Koka, Eff, and many more

