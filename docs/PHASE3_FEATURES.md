# Phase 3 Features: Safety & Metaprogramming

**Author**: Michael Lauzon

Phase 3 introduces advanced safety features and compile-time metaprogramming capabilities inspired by Eiffel, Zig, Koka, and modern effect systems.

## Overview

Phase 3 features focus on:
- **Formal Verification**: Design by Contract (requires/ensures/invariant)
- **Effect Tracking**: Explicit side effect annotations
- **Compile-time Execution**: Zig-style comptime blocks

These features enable writing safer, more maintainable code with guarantees enforced at compile time.

---

## Feature 1: Design by Contract

**Inspired by**: Eiffel (1986), D (2001), Ada (1983), Spec# (2004)

Design by Contract allows you to specify formal contracts for functions using preconditions, postconditions, and invariants.

### Syntax

```rustscript
fn function_name(params) -> return_type
    requires condition, "error message"
    ensures condition, "error message"
    invariant condition, "error message"
{
    // function body
}
```

### Preconditions (requires)

Preconditions specify what must be true before a function executes:

```rustscript
fn divide(a: number, b: number) -> number
    requires b != 0, "Divisor cannot be zero"
{
    a / b
}
```

### Postconditions (ensures)

Postconditions specify what must be true after a function executes:

```rustscript
fn sqrt(x: number) -> number
    requires x >= 0, "Cannot take square root of negative number"
    ensures result * result ≈ x, "Result squared must equal input"
    ensures result >= 0, "Result must be non-negative"
{
    Math.sqrt(x)
}
```

### Invariants

Invariants specify conditions that must always hold:

```rustscript
struct BankAccount {
    balance: number,
    account_number: string,
}

fn withdraw(account: BankAccount, amount: number) -> BankAccount
    requires amount > 0, "Withdrawal amount must be positive"
    requires account.balance >= amount, "Insufficient funds"
    ensures result.balance == account.balance - amount
    invariant result.balance >= 0, "Balance must never be negative"
{
    BankAccount {
        balance: account.balance - amount,
        account_number: account.account_number,
    }
}
```

### Multiple Contracts

Functions can have multiple preconditions and postconditions:

```rustscript
fn binary_search(arr: [number], target: number) -> number
    requires arr.is_sorted(), "Array must be sorted"
    ensures result >= -1 && result < arr.length
    ensures result == -1 || arr[result] == target
{
    // Binary search implementation
    let (left, right) = (0, arr.length - 1);
    
    while left <= right {
        let mid = (left + right) / 2;
        
        if arr[mid] == target {
            return mid;
        } else if arr[mid] < target {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    -1
}
```

### Why Design by Contract?

- **Formal Specifications**: Contracts document function behavior formally
- **Early Error Detection**: Contract violations are caught at runtime (or compile time when possible)
- **Better Testing**: Contracts serve as executable specifications
- **Self-Documenting**: Contracts make assumptions and guarantees explicit

---

## Feature 2: Effect System

**Inspired by**: Koka (2012), Eff (2012), Nim (2008), Rust traits

The effect system tracks and controls side effects, making function behavior explicit and predictable.

### Effect Annotations

```rustscript
effect [effect_list]
fn function_name(params) -> return_type {
    // function body
}
```

### Available Effects

- **pure**: No side effects (referentially transparent)
- **io**: Performs I/O operations (file, network, console)
- **state**: Modifies mutable state
- **throws**: May throw errors or panic
- **alloc**: Allocates memory
- **read**: Reads from external sources
- **write**: Writes to external sources

### Pure Functions

```rustscript
effect [pure]
fn add(a: number, b: number) -> number {
    a + b
}

effect [pure]
fn factorial(n: number) -> number {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

### I/O Effects

```rustscript
effect [io]
fn read_file(path: string) -> string {
    // Reads from file system
    fs.read(path)
}

effect [io, write]
fn write_file(path: string, content: string) {
    fs.write(path, content)
}
```

### State Effects

```rustscript
effect [state]
fn increment_counter(counter: number) -> number {
    counter + 1
}

effect [state]
fn update_cache(cache: Cache, key: string, value: any) -> Cache {
    cache.set(key, value);
    cache
}
```

### Multiple Effects

Functions can have multiple effects:

```rustscript
effect [io, throws]
fn load_config(path: string) -> Config {
    let content = read_file(path);  // IO effect
    parse_config(content)            // Throws effect
}

effect [io, state, throws]
fn process_transaction(account: BankAccount, amount: number) -> BankAccount {
    // IO: Log transaction
    console.log("Processing transaction: {amount}");
    
    // State: Modify account
    let new_account = withdraw(account, amount);
    
    // Throws: May fail if insufficient funds
    if new_account.balance < 0 {
        throw "Transaction failed";
    }
    
    new_account
}
```

### Effect Propagation

Effects propagate through the call chain:

```rustscript
effect [pure]
fn calculate(x: number) -> number {
    x * 2  // Pure function can only call pure functions
}

effect [io]
fn log_and_calculate(x: number) -> number {
    console.log("Calculating: {x}");  // IO effect
    calculate(x)  // Can call pure functions
}
```

### Why Effect Systems?

- **Explicit Side Effects**: Know what a function does at a glance
- **Safer Refactoring**: Effect changes are caught by the compiler
- **Better Reasoning**: Pure functions are easier to test and reason about
- **Controlled Impurity**: Side effects are isolated and explicit

---

## Feature 3: Compile-time Execution

**Inspired by**: Zig (2016), D CTFE (2007), C++ constexpr (2011), Nim (2008)

Compile-time execution allows running code during compilation, enabling metaprogramming and zero-cost abstractions.

### Comptime Blocks

```rustscript
comptime {
    // Code executed at compile time
}
```

### Compile-time Constants

```rustscript
comptime {
    const BUFFER_SIZE = 1024;
    const MAX_USERS = 100;
    const VERSION = "1.0.0";
}

fn create_buffer() -> [number] {
    let buffer = [];
    for i in 0..BUFFER_SIZE {  // BUFFER_SIZE known at compile time
        buffer.push(0);
    }
    buffer
}
```

### Compile-time Function Execution

```rustscript
comptime {
    fn fibonacci_at_compile_time(n: number) -> number {
        if n <= 1 {
            n
        } else {
            fibonacci_at_compile_time(n - 1) + fibonacci_at_compile_time(n - 2)
        }
    }
    
    const FIB_10 = fibonacci_at_compile_time(10);  // Computed at compile time
}

fn main() {
    console.log("Fibonacci(10) = {FIB_10}");  // No runtime computation
}
```

### Compile-time Code Generation

```rustscript
comptime {
    fn generate_accessor(field_name: string) -> string {
        "fn get_{field_name}(obj: Object) -> any { obj.{field_name} }"
    }
    
    // Generate getters at compile time
    const GET_NAME = generate_accessor("name");
    const GET_AGE = generate_accessor("age");
}
```

### Compile-time Lookup Tables

```rustscript
comptime {
    fn generate_lookup_table(size: number) -> [number] {
        let table = [];
        for i in 0..size {
            table.push(i * i);
        }
        table
    }
    
    const SQUARES = generate_lookup_table(100);
}

fn get_square(n: number) -> number {
    SQUARES[n]  // O(1) lookup, no computation
}
```

### Conditional Compilation

```rustscript
comptime {
    const DEBUG = true;
    const OPTIMIZATION_LEVEL = 2;
    
    if DEBUG {
        fn log_debug(message: string) {
            console.log("[DEBUG] {message}");
        }
    } else {
        fn log_debug(message: string) {
            // No-op in release mode
        }
    }
}
```

### Compile-time Type Checking

```rustscript
comptime {
    fn assert_type<T>(value: T, expected_type: string) {
        if typeof(value) != expected_type {
            compile_error("Type mismatch: expected {expected_type}");
        }
    }
}
```

### Why Compile-time Execution?

- **Zero Runtime Cost**: Computation happens at compile time
- **Metaprogramming**: Generate code based on compile-time information
- **Type-Safe**: Errors caught during compilation
- **Performance**: Pre-computed values and optimised code

---

## Combined Example: Safe Database Operations

Here's a real-world example combining all Phase 3 features:

```rustscript
// Compile-time configuration
comptime {
    const MAX_RETRIES = 3;
    const TIMEOUT_MS = 5000;
    const DEBUG = true;
}

struct Database {
    connection_string: string,
    connected: bool,
}

// Effect-tracked function with contracts
effect [io, throws]
fn connect(db: Database) -> Database
    requires db.connection_string.length > 0
    ensures result.connected == true
{
    console.log("Connecting to database...");
    Database {
        connection_string: db.connection_string,
        connected: true,
    }
}

effect [io, state, throws]
fn execute_query(db: Database, query: string) -> [any]
    requires db.connected, "Database must be connected"
    requires query.length > 0, "Query cannot be empty"
    ensures result.length >= 0
{
    console.log("Executing: {query}");
    []
}

// Compile-time query validation
comptime {
    fn validate_sql(query: string) -> bool {
        let dangerous_keywords = ["DROP", "DELETE", "TRUNCATE"];
        for keyword in dangerous_keywords {
            if query.uppercase().contains(keyword) {
                return false;
            }
        }
        true
    }
}

effect [io, state, throws]
fn safe_database_operation() {
    let db = Database {
        connection_string: "postgresql://localhost/mydb",
        connected: false,
    };
    
    // Connect with contract checking
    db = connect(db);
    
    // Compile-time validated query
    comptime {
        const QUERY = "SELECT * FROM users WHERE active = true";
        if !validate_sql(QUERY) {
            compile_error("Dangerous SQL query detected");
        }
    }
    
    // Execute with effect tracking
    let results = execute_query(db, QUERY);
    console.log("Found {results.length} results");
    
    // Ensure cleanup with defer
    defer {
        db = disconnect(db);
    }
}
```

---

## Feature Summary

| Feature | Purpose | Benefit |
|---------|---------|---------|
| **Design by Contract** | Formal specifications | Catch bugs early, self-documenting code |
| **Effect System** | Track side effects | Explicit behavior, safer refactoring |
| **Compile-time Execution** | Metaprogramming | Zero runtime cost, type-safe generation |

---

## Language Heritage

Phase 3 features draw from decades of research in formal methods and metaprogramming:

- **Eiffel (1986)**: Pioneered Design by Contract
- **Ada (1983)**: Contract-based programming
- **D (2001)**: Compile-time function execution (CTFE)
- **Koka (2012)**: Modern effect system
- **Zig (2016)**: Comptime metaprogramming
- **Nim (2008)**: Effect system and compile-time execution

---

## See Also

- [Phase 1 Features](PHASE1_FEATURES.md) - String & syntax enhancements
- [Phase 2 Features](PHASE2_FEATURES.md) - Function enhancements
- [Examples](../examples/phase3_features.rscc) - Working code examples
