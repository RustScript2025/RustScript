# Phase 1 Features: String & Syntax Enhancements

**Author**: Michael Lauzon

This document describes the Phase 1 features added to RustScript, focusing on ergonomic improvements for string handling and collection manipulation.

## Overview

Phase 1 introduces four major features that significantly improve code readability and reduce boilerplate:

1. **String Interpolation** - Embed expressions directly in strings
2. **Optional Chaining** - Safe navigation through potentially null values
3. **Null Coalescing** - Provide default values for null/undefined
4. **List Comprehensions** - Concise syntax for transforming collections

## Feature 1: String Interpolation

### Syntax

```rust
let name = "Alice";
let age = 30;
let message = "Hello, {name}! You are {age} years old.";
```

### Description

String interpolation allows you to embed expressions directly within string literals using `{expression}` syntax. This is more readable and maintainable than string concatenation.

### Benefits

- **Readability**: Intent is clear at a glance
- **Type Safety**: Expressions are type-checked
- **Performance**: Compiled efficiently (no runtime string parsing)

### Examples

```rust
// Simple variable interpolation
let greeting = "Hello, {name}!";

// Expression evaluation
let calculation = "2 + 2 = {2 + 2}";

// Method calls
let upper = "Name: {name.uppercase()}";

// Complex expressions
let summary = "User {user.name} (ID: {user.id}) logged in at {timestamp.format()}";
```

### Implementation Notes

- Expressions within `{}` are evaluated at runtime
- All expressions are converted to strings automatically
- Nested braces require escaping: `\{` and `\}`
- Empty braces `{}` are invalid

## Feature 2: Optional Chaining

### Syntax

```rust
let value = object?.property?.nested_property;
```

### Description

Optional chaining provides safe navigation through object properties that might be null or undefined. If any part of the chain is null, the entire expression evaluates to null without throwing an error.

### Benefits

- **Safety**: Eliminates null pointer exceptions
- **Conciseness**: Reduces nested if-let statements
- **Clarity**: Intent is explicit in the syntax

### Examples

```rust
// Without optional chaining
let street = if let Some(user) = user {
    if let Some(address) = user.address {
        Some(address.street)
    } else {
        None
    }
} else {
    None
};

// With optional chaining
let street = user?.address?.street;

// Method calls
let length = user?.name?.length();

// Array access
let first_item = cart?.items[0]?.name;
```

### Behaviour

- Returns `Option<T>` where `T` is the type of the final property
- Short-circuits on first null value
- Can be chained indefinitely
- Works with methods, properties, and array indices

## Feature 3: Null Coalescing

### Syntax

```rust
let value = potentially_null ?? default_value;
```

### Description

The null coalescing operator `??` provides a default value when the left-hand expression is null or undefined. This is cleaner than using match expressions or if-let for simple default cases.

### Benefits

- **Simplicity**: One operator instead of control flow
- **Composability**: Can be chained
- **Readability**: Intent is immediately clear

### Examples

```rust
// Simple default
let name = user_name ?? "Anonymous";

// Chaining multiple fallbacks
let value = first_option() ?? second_option() ?? "default";

// Combined with optional chaining
let city = user?.address?.city ?? "Unknown";

// Function calls
let config = load_config() ?? default_config();
```

### Behaviour

- Only evaluates right side if left is null/undefined
- Right side can be any expression
- Can be chained left-to-right
- Different from `||` which checks for falsy values

### Comparison with OR Operator

```rust
// ?? only checks for null/undefined
let value = 0 ?? 42;  // Returns 0

// || checks for falsy values
let value = 0 || 42;  // Returns 42
```

## Feature 4: List Comprehensions

### Syntax

```rust
let result = [expression for pattern in iterator if condition];
```

### Description

List comprehensions provide a concise, readable syntax for transforming and filtering collections. They're inspired by Python but with Rust's pattern matching capabilities.

### Benefits

- **Conciseness**: One line instead of map/filter chains
- **Readability**: Reads like natural language
- **Performance**: Compiled to efficient loops

### Examples

```rust
// Basic transformation
let doubled = [x * 2 for x in numbers];

// With filter
let evens = [x for x in numbers if x % 2 == 0];

// Complex transformation
let squares_of_evens = [x * x for x in numbers if x % 2 == 0];

// Pattern matching
let names = [(id, user.name) for (id, user) in users];

// Nested comprehensions
let pairs = [(x, y) for x in [1, 2, 3] for y in [4, 5, 6]];

// String manipulation
let uppercase_names = [name.uppercase() for name in names];
```

### Syntax Components

1. **Expression**: What to compute for each element
2. **Pattern**: How to destructure each element
3. **Iterator**: The collection to iterate over
4. **Condition** (optional): Filter predicate

### Comparison with Traditional Approaches

```rust
// Traditional approach
let result = numbers
    .iter()
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .collect();

// List comprehension
let result = [x * x for x in numbers if x % 2 == 0];
```

### Advanced Patterns

```rust
// Destructuring tuples
let sums = [x + y for (x, y) in pairs];

// Nested patterns
let flattened = [item for sublist in lists for item in sublist];

// Multiple conditions
let filtered = [x for x in numbers if x > 0 if x < 100];

// With type annotations
let typed: Vec<i32> = [x as i32 for x in floats];
```

## Combined Usage Examples

### Example 1: Data Processing

```rust
struct User {
    name: string,
    email: string,
    age: number,
}

fn process_users(users: [User]) {
    // Filter adults, extract names, format with interpolation
    let adult_names = [
        "Name: {user.name}, Email: {user.email}"
        for user in users
        if user.age >= 18
    ];
    
    // Safe access with optional chaining and defaults
    let first_adult = adult_names[0] ?? "No adults found";
    
    console.log(first_adult);
}
```

### Example 2: Configuration Management

```rust
fn load_settings() {
    // Try multiple sources with null coalescing
    let config = load_from_file() ?? load_from_env() ?? default_config();
    
    // Safe property access
    let db_host = config?.database?.host ?? "localhost";
    let db_port = config?.database?.port ?? 5432;
    
    // Format connection string
    let connection = "postgresql://{db_host}:{db_port}/mydb";
    
    console.log("Connecting to: {connection}");
}
```

### Example 3: Log Processing

```rust
fn analyse_logs(logs: [LogEntry]) {
    // Extract error messages with comprehension
    let errors = [
        "{log.timestamp}: {log.message}"
        for log in logs
        if log.level == "ERROR"
    ];
    
    // Get unique user IDs with optional chaining
    let user_ids = [
        log?.user_id ?? "anonymous"
        for log in errors
    ];
    
    // Summary with interpolation
    let summary = "Found {errors.length} errors from {user_ids.length} users";
    console.log(summary);
}
```

## Migration Guide

### From String Concatenation

```rust
// Before
let message = "Hello, " + name + "! You are " + age.to_string() + " years old.";

// After
let message = "Hello, {name}! You are {age} years old.";
```

### From Nested If-Let

```rust
// Before
let street = match user {
    Some(u) => match u.address {
        Some(a) => Some(a.street),
        None => None,
    },
    None => None,
};

// After
let street = user?.address?.street;
```

### From Match for Defaults

```rust
// Before
let name = match user_name {
    Some(n) => n,
    None => "Anonymous",
};

// After
let name = user_name ?? "Anonymous";
```

### From Iterator Chains

```rust
// Before
let result: Vec<_> = numbers
    .iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * x)
    .collect();

// After
let result = [x * x for x in numbers if x % 2 == 0];
```

## Performance Considerations

### String Interpolation

- Compiled to efficient string concatenation
- No runtime parsing overhead
- Expressions evaluated once

### Optional Chaining

- Short-circuits on first null
- No performance penalty vs manual checks
- Compiles to efficient null checks

### Null Coalescing

- Lazy evaluation of right side
- Only evaluates default if needed
- Zero overhead vs if-else

### List Comprehensions

- Compiles to efficient loops
- No intermediate allocations
- Comparable to hand-written loops

## Best Practices

### String Interpolation

✅ **Do:**
```rust
let message = "User {user.name} logged in at {timestamp}";
```

❌ **Don't:**
```rust
let message = "User " + user.name + " logged in at " + timestamp;
```

### Optional Chaining

✅ **Do:**
```rust
let value = obj?.prop?.nested;
```

❌ **Don't:**
```rust
let value = if let Some(o) = obj {
    if let Some(p) = o.prop {
        p.nested
    } else { None }
} else { None };
```

### Null Coalescing

✅ **Do:**
```rust
let name = user_name ?? "Anonymous";
```

❌ **Don't:**
```rust
let name = match user_name {
    Some(n) => n,
    None => "Anonymous",
};
```

### List Comprehensions

✅ **Do:**
```rust
let evens = [x for x in numbers if x % 2 == 0];
```

❌ **Don't:**
```rust
let evens: Vec<_> = numbers.iter().filter(|&&x| x % 2 == 0).collect();
```

## Limitations and Future Work

### Current Limitations

1. **String Interpolation**: Complex expressions may require parentheses
2. **Optional Chaining**: Limited to property access (no method chaining yet)
3. **List Comprehensions**: Nested comprehensions have depth limits
4. **Type Inference**: May require explicit types in some cases

### Planned Enhancements

- **Phase 2**: Generator expressions with `yield`
- **Phase 3**: Async comprehensions
- **Phase 4**: Pattern guards in comprehensions
- **Phase 5**: Custom interpolation formatters

## See Also

- [Phase 2 Features](PHASE2_FEATURES.md) - Function enhancements
- [Language Reference](LANGUAGE_REFERENCE.md) - Complete syntax guide
- [Examples](../examples/) - More code examples
