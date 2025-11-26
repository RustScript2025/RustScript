# RustScript Tutorial: A Complete Guide

**Author**: Michael Lauzon

Welcome to RustScript! This tutorial will take you from complete beginner to confident RustScript developer. We'll start with the absolute basics and gradually introduce more advanced concepts, explaining not just *what* to do, but *why* and *how* things work.

## What You'll Learn

By the end of this tutorial, you'll understand:
- How RustScript combines the best features from 60+ years of scripting language evolution (plus Rust's memory safety)
- Why immutability by default makes your code safer
- How to write expressive, maintainable code using modern syntax
- When to use advanced features like generators and multiple dispatch
- How to build production-ready applications with contracts and effect systems

## Prerequisites

This tutorial assumes you have:
- Basic programming knowledge (variables, functions, loops)
- Familiarity with at least one programming language
- Rust 1.91.1+ installed on your system (with Edition 2024 support)
- A text editor (VS Code, Sublime, Vim, etc.)

Don't worry if you're not a Rust expert - RustScript is designed to be approachable!

**About Rust Edition 2024**: RustScript is built using the latest Rust Edition 2024, which brings modern features like implicit format arguments, improved error handling, and enhanced pattern matching. This ensures the compiler itself uses cutting-edge Rust practices.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Hello, World! - Your First Programme](#hello-world---your-first-programme)
3. [Understanding Variables and Immutability](#understanding-variables-and-immutability)
4. [Functions: The Building Blocks](#functions-the-building-blocks)
5. [Control Flow: Making Decisions](#control-flow-making-decisions)
6. [Data Structures: Organizing Information](#data-structures-organizing-information)
7. [Pattern Matching: Elegant Control Flow](#pattern-matching-elegant-control-flow)
8. [Error Handling: Dealing with Failure](#error-handling-dealing-with-failure)
9. [Phase 1: Modern Syntax Enhancements](#phase-1-modern-syntax-enhancements)
10. [Phase 2: Advanced Function Capabilities](#phase-2-advanced-function-capabilities)
11. [Phase 3: Safety and Metaprogramming](#phase-3-safety-and-metaprogramming)
12. [Building Real Applications](#building-real-applications)

---

## Getting Started

### Installing RustScript

First, let's get RustScript installed on your system. Open your terminal and run:

```bash
# Clone the repository
git clone https://github.com/RustScript2025/RustScript.git
cd RustScript

# Build the compiler
cargo build --release
```

This will take a few minutes as Cargo downloads dependencies and compiles the compiler. When it's done, you'll find the `rjsc` compiler at `target/release/rjsc` (or `rjsc.exe` on Windows).

**What just happened?**
- `cargo build` compiles the RustScript compiler (which is written in Rust)
- `--release` creates an optimised version for better performance
- The compiler is now ready to transform your `.rjsc` files into JavaScript or WebAssembly

### Setting Up Your First Project

Create a new directory for your RustScript projects:

```bash
mkdir rustscript-learning
cd rustscript-learning
```

Now you're ready to write your first programme!

---

## Hello, World! - Your First Programme

Every programming journey begins with "Hello, World!" - a simple programme that displays text. This tradition dates back to the 1970s and helps you verify that everything is working correctly.

### Writing the Programme

Create a file called `hello.rjsc` and type:

```rustscript
fn main() {
    console.log("Hello, World!");
}

main();
```

### Understanding Each Line

Let's break down what each part means:

**Line 1: `fn main() {`**
- `fn` is the keyword that declares a function (short for "function")
- `main` is the name we're giving this function
- `()` means this function takes no parameters (no input)
- `{` starts the function body (the code that runs when we call this function)

**Line 2: `console.log("Hello, World!");`**
- `console.log()` is a function that prints text to the console
- This comes from JavaScript - RustScript can use JavaScript functions!
- `"Hello, World!"` is a string (text) we want to display
- `;` ends the statement (like a period ends a sentence)

**Line 3: `}`**
- Closes the function body we started with `{`

**Line 5: `main();`**
- This actually calls (runs) the function we defined
- Without this line, we'd define the function but never execute it
- `()` means we're not passing any arguments to the function

### Running Your Programme

Now let's compile and run it:

```bash
# Compile to JavaScript
../target/release/rjsc hello.rjsc

# Run with Node.js
node hello.js
```

You should see:
```
Hello, World!
```

**What happened behind the scenes?**
1. The RustScript compiler (`rjsc`) read your `.rjsc` file
2. It parsed the code and checked for errors
3. It generated equivalent JavaScript code in `hello.js`
4. Node.js executed that JavaScript
5. The text appeared in your terminal

### Your First WebAssembly Programme

RustScript can also compile to WebAssembly, which runs in web browsers. Let's try it!

Create `hello.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Hello RustScript</title>
</head>
<body>
    <h1>RustScript in the Browser</h1>
    <p>Check the browser console (F12) to see the output!</p>
    
    <script type="text/rustscript">
        fn main() {
            console.log("Hello from WebAssembly!");
        }
        main();
    </script>
</body>
</html>
```

**What's different here?**
- The RustScript code is embedded in HTML using `<script type="text/rustscript">`
- The browser's RustScript runtime will automatically compile and execute it
- The output appears in the browser's developer console (press F12 to see it)

To view this:
```bash
# Start the development server
python ../serve.py
```

Then open `http://localhost:8000/hello.html` in your browser and press F12 to see the console output.

**Why two targets (JavaScript and WebAssembly)?**
- **JavaScript**: Great for Node.js servers, command-line tools, and quick prototyping
- **WebAssembly**: Faster execution in browsers, better for performance-critical web apps

---

## Understanding Variables and Immutability

Variables store data that your programme can use and manipulate. RustScript has a unique approach to variables that prevents many common bugs.

### Immutable Variables: The Default

In RustScript, variables are **immutable by default**. This means once you assign a value, you can't change it:

```rustscript
fn main() {
    let name = "Alice";
    console.log("Hello, {name}!");
    
    // This would cause a compile error:
    // name = "Bob";  // ERROR: Cannot assign to immutable variable
}
```

**Why immutable by default?**

This might seem restrictive, but it's actually a powerful safety feature:

1. **Prevents Accidental Changes**: You can't accidentally modify a value somewhere in your code
2. **Makes Code Easier to Reason About**: When you see `let x = 5`, you know `x` will always be 5
3. **Enables Optimisations**: The compiler can make assumptions about unchanging values
4. **Reduces Bugs**: Many bugs come from unexpected state changes

**Real-world example:**
```rustscript
fn calculate_total(price: number, tax_rate: number) -> number {
    let subtotal = price;
    let tax = subtotal * tax_rate;
    let total = subtotal + tax;
    
    // Later in the function, you know these values haven't changed
    // No need to worry about someone modifying subtotal or tax
    
    total
}
```

### Mutable Variables: When You Need to Change

Sometimes you *do* need to change a value. Use `let mut` to make a variable mutable:

```rustscript
fn main() {
    let mut count = 0;
    console.log("Count starts at: {count}");
    
    count = count + 1;
    console.log("After adding 1: {count}");
    
    count += 1;  // Shorthand for count = count + 1
    console.log("After adding 1 again: {count}");
}
```

**Output:**
```
Count starts at: 0
After adding 1: 1
After adding 1 again: 2
```

**When to use `mut`:**
- Counters and accumulators
- Building up data structures
- Iterative algorithms
- Any time you need to update a value

**Best practice:** Start with immutable (`let`) and only add `mut` when you need it. This makes your intent clear: "This value will change."

### Type Annotations: Being Explicit

RustScript can usually figure out what type a variable is (this is called **type inference**):

```rustscript
fn main() {
    let age = 30;        // RustScript knows this is a number
    let name = "Alice";  // RustScript knows this is a string
    let active = true;   // RustScript knows this is a boolean
}
```

But you can be explicit if you want:

```rustscript
fn main() {
    let age: number = 30;
    let name: string = "Alice";
    let active: bool = true;
    
    console.log("Age: {age}, Name: {name}, Active: {active}");
}
```

**When to use type annotations:**
- When the type isn't obvious from the value
- For function parameters (always required)
- For function return types (recommended)
- When you want to make your code more self-documenting

### Constants: Compile-Time Values

Constants are like immutable variables, but they're evaluated at compile time:

```rustscript
const MAX_USERS = 100;
const PI = 3.14159;
const APP_NAME = "MyApp";

fn main() {
    console.log("Maximum users: {MAX_USERS}");
    console.log("Pi: {PI}");
    console.log("Application: {APP_NAME}");
}
```

**Difference between `let` and `const`:**
- `let`: Value determined at runtime (when the programme runs)
- `const`: Value determined at compile time (when you build the programme)
- `const`: Must be a literal value or compile-time expression
- `const`: Slightly more efficient (value is baked into the compiled code)

**When to use constants:**
- Configuration values that never change
- Mathematical constants
- Maximum/minimum limits
- Application metadata

---


## Functions: The Building Blocks

Functions are reusable blocks of code that perform specific tasks. They're the fundamental building blocks of any programme.

### Basic Function Syntax

Here's the anatomy of a function:

```rustscript
fn greet(name: string) {
    console.log("Hello, {name}!");
}

fn main() {
    greet("Alice");
    greet("Bob");
    greet("Charlie");
}
```

**Breaking it down:**
- `fn` declares a function
- `greet` is the function name (use descriptive names!)
- `(name: string)` is a parameter - input the function needs
- `name` is the parameter name
- `string` is the parameter type
- The code between `{` and `}` is what runs when you call the function

**Why use functions?**

1. **Avoid Repetition**: Write code once, use it many times
2. **Organization**: Break complex problems into smaller pieces
3. **Readability**: `greet("Alice")` is clearer than repeating the console.log code
4. **Testing**: Easier to test small, focused functions
5. **Maintenance**: Fix bugs in one place, not scattered throughout your code

### Functions with Return Values

Functions can give back (return) a value:

```rustscript
fn add(a: number, b: number) -> number {
    a + b
}

fn main() {
    let sum = add(5, 3);
    console.log("5 + 3 = {sum}");
    
    let result = add(10, 20);
    console.log("10 + 20 = {result}");
}
```

**Key points about returns:**

1. **Return Type**: `-> number` tells us this function returns a number
2. **Implicit Return**: The last expression (without `;`) is automatically returned
3. **No `return` Keyword Needed**: `a + b` is returned automatically

**Why implicit returns?**

This is inspired by functional programming languages. It encourages thinking of functions as expressions that evaluate to values, rather than procedures that execute steps.

Compare these equivalent functions:

```rustscript
// RustScript style (implicit return)
fn multiply(a: number, b: number) -> number {
    a * b
}

// Traditional style (explicit return)
fn multiply_explicit(a: number, b: number) -> number {
    return a * b;
}
```

Both work, but the first is more concise. Use explicit `return` when you need to return early:

```rustscript
fn divide(a: number, b: number) -> number {
    if b == 0.0 {
        console.error("Cannot divide by zero!");
        return 0.0;  // Early return
    }
    
    a / b  // Implicit return for normal case
}
```

### Named Arguments: Making Calls Clear

When functions have multiple parameters, it can be hard to remember what each one means:

```rustscript
// What do these booleans mean?
create_user("Alice", 30, true, false);
```

Named arguments solve this:

```rustscript
fn create_user(name: string, age: number, admin: bool, active: bool) {
    console.log("Creating user:");
    console.log("  Name: {name}");
    console.log("  Age: {age}");
    console.log("  Admin: {admin}");
    console.log("  Active: {active}");
}

fn main() {
    // Much clearer what each argument means!
    create_user(
        name: "Alice",
        age: 30,
        admin: true,
        active: false
    );
}
```

**Benefits of named arguments:**
- **Self-Documenting**: The call site explains itself
- **Prevents Mistakes**: Can't accidentally swap arguments
- **Easier to Maintain**: Adding parameters doesn't break existing calls

### Multiple Return Values with Tuples

Sometimes you need to return more than one value:

```rustscript
fn get_user_info() -> (string, number, bool) {
    let name = "Alice";
    let age = 30;
    let active = true;
    
    (name, age, active)  // Return a tuple
}

fn main() {
    let (name, age, active) = get_user_info();
    
    console.log("Name: {name}");
    console.log("Age: {age}");
    console.log("Active: {active}");
}
```

**Understanding tuples:**
- A tuple groups multiple values together
- `(string, number, bool)` is a tuple type
- `(name, age, active)` creates a tuple value
- `let (name, age, active) = ...` destructures (unpacks) the tuple

---

## Control Flow: Making Decisions

Programmes need to make decisions and repeat actions. Control flow structures let you do this.

### If Expressions: Making Choices

The `if` expression lets your programme choose between different paths:

```rustscript
fn main() {
    let age = 20;
    
    if age >= 18 {
        console.log("You are an adult");
    } else {
        console.log("You are a minor");
    }
}
```

**Why "if expression" not "if statement"?**

In RustScript, `if` is an expression - it produces a value:

```rustscript
fn main() {
    let age = 20;
    
    let status = if age >= 18 {
        "adult"
    } else {
        "minor"
    };
    
    console.log("Status: {status}");
}
```

This is more concise than:

```rustscript
// Traditional approach
let status;
if age >= 18 {
    status = "adult";
} else {
    status = "minor";
}
```

**Multiple conditions:**

```rustscript
fn classify_temperature(temp: number) -> string {
    if temp < 0 {
        "freezing"
    } else if temp < 20 {
        "cold"
    } else if temp < 30 {
        "warm"
    } else {
        "hot"
    }
}

fn main() {
    console.log("10°C is {classify_temperature(10)}");
    console.log("25°C is {classify_temperature(25)}");
}
```

### Loops: Repeating Actions

Loops let you repeat code multiple times.

#### The `loop` Loop: Infinite Repetition

The simplest loop runs forever (until you `break` out):

```rustscript
fn main() {
    let mut count = 0;
    
    loop {
        console.log("Count: {count}");
        count += 1;
        
        if count >= 5 {
            break;  // Exit the loop
        }
    }
    
    console.log("Done!");
}
```

**Output:**
```
Count: 0
Count: 1
Count: 2
Count: 3
Count: 4
Done!
```

**When to use `loop`:**
- When you don't know how many iterations you need
- For event loops or servers that run indefinitely
- When the exit condition is complex

#### The `while` Loop: Conditional Repetition

`while` loops run as long as a condition is true:

```rustscript
fn main() {
    let mut count = 0;
    
    while count < 5 {
        console.log("Count: {count}");
        count += 1;
    }
    
    console.log("Done!");
}
```

This is equivalent to the `loop` example above, but more concise when you have a simple condition.

**When to use `while`:**
- When you have a clear condition to check
- For countdown timers
- For processing until a flag changes

#### The `for` Loop: Iterating Over Collections

`for` loops iterate over collections (arrays, ranges, etc.):

```rustscript
fn main() {
    let fruits = ["apple", "banana", "cherry"];
    
    for fruit in fruits {
        console.log("I like {fruit}");
    }
}
```

**Output:**
```
I like apple
I like banana
I like cherry
```

**Iterating over ranges:**

```rustscript
fn main() {
    // Print numbers 0 through 4
    for i in 0..5 {
        console.log("Number: {i}");
    }
    
    // Print numbers 1 through 5 (inclusive)
    for i in 1..=5 {
        console.log("Number: {i}");
    }
}
```

**When to use `for`:**
- When iterating over arrays, lists, or other collections
- When you need to do something a specific number of times
- Most common type of loop in practice

### Loop Control: `break` and `continue`

Control loop execution with `break` and `continue`:

```rustscript
fn main() {
    // Skip even numbers, stop at 8
    for i in 0..10 {
        if i % 2 == 0 {
            continue;  // Skip to next iteration
        }
        
        if i >= 8 {
            break;  // Exit loop entirely
        }
        
        console.log("Odd number: {i}");
    }
}
```

**Output:**
```
Odd number: 1
Odd number: 3
Odd number: 5
Odd number: 7
```

---

## Data Structures: Organising Information

Real programmes work with complex data. Data structures help you organise and manage this information.

### Arrays: Lists of Values

Arrays store multiple values of the same type:

```rustscript
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    let names = ["Alice", "Bob", "Charlie"];
    
    // Access elements by index (starting at 0)
    console.log("First number: {numbers[0]}");
    console.log("Second name: {names[1]}");
    console.log("Last number: {numbers[4]}");
}
```

**Understanding arrays:**
- All elements must be the same type
- Fixed size (can't grow or shrink after creation)
- Zero-indexed (first element is at position 0)
- Fast access to any element

**Working with arrays:**

```rustscript
fn main() {
    let mut scores = [85, 92, 78, 95, 88];
    
    // Get array length
    console.log("Number of scores: {scores.length}");
    
    // Modify an element (array must be mutable)
    scores[0] = 90;
    console.log("Updated first score: {scores[0]}");
    
    // Iterate over array
    for score in scores {
        console.log("Score: {score}");
    }
}
```

### Tuples: Fixed Collections of Different Types

Tuples group values of different types together:

```rustscript
fn main() {
    // A tuple with three different types
    let person = ("Alice", 30, true);
    
    // Access by position
    console.log("Name: {person.0}");
    console.log("Age: {person.1}");
    console.log("Active: {person.2}");
    
    // Destructure into separate variables
    let (name, age, active) = person;
    console.log("Name: {name}, Age: {age}, Active: {active}");
}
```

**When to use tuples:**
- Returning multiple values from a function
- Grouping related values temporarily
- When you don't need named fields (use structs for that)

### Structs: Custom Data Types

Structs let you create your own data types with named fields:

```rustscript
struct User {
    name: string,
    email: string,
    age: number,
    active: bool
}

fn main() {
    let user = User {
        name: "Alice",
        email: "alice@example.com",
        age: 30,
        active: true
    };
    
    console.log("User: {user.name}");
    console.log("Email: {user.email}");
    console.log("Age: {user.age}");
}
```

**Why structs are better than tuples:**

Compare these two approaches:

```rustscript
// With tuple - what does each position mean?
let user = ("Alice", "alice@example.com", 30, true);
console.log(user.0);  // Is this name or email?

// With struct - crystal clear!
let user = User {
    name: "Alice",
    email: "alice@example.com",
    age: 30,
    active: true
};
console.log(user.name);  // Obviously the name!
```

### Adding Behaviour with `impl`

Structs hold data. Methods (functions associated with structs) add behaviour:

```rustscript
struct Counter {
    count: number
}

impl Counter {
    // Constructor (by convention, called "new")
    fn new() -> Counter {
        Counter { count: 0 }
    }
    
    // Method that modifies the counter
    fn increment(&mut this) {
        this.count += 1;
    }
    
    // Method that reads the counter
    fn get(&this) -> number {
        this.count
    }
    
    // Method that resets the counter
    fn reset(&mut this) {
        this.count = 0;
    }
}

fn main() {
    let mut counter = Counter::new();
    
    console.log("Initial: {counter.get()}");
    
    counter.increment();
    counter.increment();
    console.log("After 2 increments: {counter.get()}");
    
    counter.reset();
    console.log("After reset: {counter.get()}");
}
```

**Understanding `this`:**
- `&this` means "borrow this struct" (read-only access)
- `&mut this` means "borrow this struct mutably" (can modify it)
- `this` is like `self` in Python or `this` in JavaScript

**Static methods vs instance methods:**

```rustscript
impl Counter {
    // Static method - called on the type itself
    fn new() -> Counter {
        Counter { count: 0 }
    }
    
    // Instance method - called on a specific counter
    fn increment(&mut this) {
        this.count += 1;
    }
}

fn main() {
    let mut counter = Counter::new();  // Static method
    counter.increment();                // Instance method
}
```

---


## Pattern Matching: Elegant Control Flow

Pattern matching is one of RustScript's most powerful features. It lets you handle different cases elegantly and safely.

### Basic Match Expressions

`match` is like a supercharged `switch` statement:

```rustscript
fn describe_number(n: number) -> string {
    match n {
        0 => "zero",
        1 => "one",
        2 => "two",
        3 => "three",
        _ => "many"
    }
}

fn main() {
    console.log(describe_number(0));   // Prints: zero
    console.log(describe_number(2));   // Prints: two
    console.log(describe_number(10));  // Prints: many
}
```

**Key differences from `switch`:**

1. **No Fallthrough**: Each arm is independent (no need for `break`)
2. **Returns a Value**: `match` is an expression
3. **Exhaustive**: Must handle all possible cases
4. **Pattern Matching**: Can match complex patterns, not just values

**The wildcard pattern `_`:**

The underscore `_` matches anything. It's like the `default` case in a `switch` statement:

```rustscript
match value {
    1 => "one",
    2 => "two",
    _ => "something else"  // Catches everything else
}
```

### Range Matching

Match ranges of values:

```rustscript
fn classify_age(age: number) -> string {
    match age {
        0..=12 => "child",      // 0 to 12 inclusive
        13..=19 => "teenager",  // 13 to 19 inclusive
        20..=64 => "adult",     // 20 to 64 inclusive
        _ => "senior"           // 65 and above
    }
}

fn main() {
    console.log("Age 10: {classify_age(10)}");  // child
    console.log("Age 16: {classify_age(16)}");  // teenager
    console.log("Age 30: {classify_age(30)}");  // adult
    console.log("Age 70: {classify_age(70)}");  // senior
}
```

**Understanding range syntax:**
- `0..5` means 0, 1, 2, 3, 4 (excludes 5)
- `0..=5` means 0, 1, 2, 3, 4, 5 (includes 5)
- Use `..=` in match expressions for inclusive ranges

### Destructuring Patterns

Match and extract values from complex data:

```rustscript
fn main() {
    let point = (10, 20);
    
    match point {
        (0, 0) => console.log("Origin"),
        (x, 0) => console.log("On x-axis at {x}"),
        (0, y) => console.log("On y-axis at {y}"),
        (x, y) => console.log("Point at ({x}, {y})")
    }
}
```

**What's happening here:**
1. First pattern checks if both coordinates are 0
2. Second pattern checks if y is 0, captures x value
3. Third pattern checks if x is 0, captures y value
4. Fourth pattern matches anything, captures both values

**Destructuring structs:**

```rustscript
struct User {
    name: string,
    age: number
}

fn greet_user(user: User) {
    match user {
        User { name: "Admin", age: _ } => {
            console.log("Welcome, Administrator!");
        },
        User { name, age } if age < 18 => {
            console.log("Hello, {name}! You're a minor.");
        },
        User { name, age } => {
            console.log("Hello, {name}! You're {age} years old.");
        }
    }
}
```

### Guards: Adding Conditions

Guards let you add extra conditions to patterns:

```rustscript
fn classify_number(n: number) -> string {
    match n {
        x if x < 0 => "negative",
        0 => "zero",
        x if x % 2 == 0 => "positive even",
        _ => "positive odd"
    }
}

fn main() {
    console.log(classify_number(-5));  // negative
    console.log(classify_number(0));   // zero
    console.log(classify_number(4));   // positive even
    console.log(classify_number(7));   // positive odd
}
```

**Understanding guards:**
- `if` after a pattern adds a condition
- The pattern must match AND the condition must be true
- Guards make patterns more expressive

---

## Error Handling: Dealing with Failure

Things go wrong in programmes. Good error handling makes your code robust and maintainable.

### The Problem with Exceptions

Many languages use exceptions for error handling:

```javascript
// JavaScript style (what we're avoiding)
try {
    let result = divide(10, 0);
    console.log(result);
} catch (error) {
    console.error("Error:", error);
}
```

**Problems with exceptions:**
1. **Invisible Control Flow**: You can't tell from the function signature if it throws
2. **Easy to Ignore**: Forgot to catch? Your programme crashes
3. **Hard to Track**: Exceptions can jump across many function calls
4. **Performance**: Exception handling has overhead

### The Result Type: Explicit Error Handling

RustScript uses the `Result` type to make errors explicit:

```rustscript
fn divide(a: number, b: number) -> Result<number, string> {
    if b == 0.0 {
        Err("Cannot divide by zero")
    } else {
        Ok(a / b)
    }
}

fn main() {
    match divide(10.0, 2.0) {
        Ok(result) => console.log("Result: {result}"),
        Err(error) => console.error("Error: {error}")
    }
}
```

**Understanding Result:**
- `Result<T, E>` is a type that can be either:
  - `Ok(value)` - Success, contains a value of type T
  - `Err(error)` - Failure, contains an error of type E
- You must handle both cases (compiler enforces this)
- Errors are values, not exceptions

**Why this is better:**

```rustscript
// The function signature tells you it can fail
fn divide(a: number, b: number) -> Result<number, string>

// You MUST handle both cases
match divide(10.0, 0.0) {
    Ok(result) => { /* handle success */ },
    Err(error) => { /* handle error */ }
}
```

### The Option Type: Handling Missing Values

`Option` represents a value that might not exist:

```rustscript
fn find_user(id: number) -> Option<string> {
    if id == 1 {
        Some("Alice")
    } else if id == 2 {
        Some("Bob")
    } else {
        None
    }
}

fn main() {
    match find_user(1) {
        Some(name) => console.log("Found user: {name}"),
        None => console.log("User not found")
    }
    
    match find_user(999) {
        Some(name) => console.log("Found user: {name}"),
        None => console.log("User not found")
    }
}
```

**Output:**
```
Found user: Alice
User not found
```

**When to use Option vs Result:**
- **Option**: When absence is normal (user not found, optional field)
- **Result**: When absence is an error (file not found, network failure)

### Chaining Operations

Handle multiple operations that might fail:

```rustscript
fn get_user_age(id: number) -> Option<number> {
    let user = find_user(id)?;  // Return None if user not found
    let age = parse_age(user)?;  // Return None if age invalid
    Some(age)
}
```

The `?` operator:
- If the value is `Some(x)` or `Ok(x)`, unwrap it
- If the value is `None` or `Err(e)`, return early with that error
- Makes error handling concise

---

## Phase 1: Modern Syntax Enhancements

Now we get to RustScript's unique features! Phase 1 adds modern syntax that makes code more readable and maintainable.

### String Interpolation: Embedding Expressions

**The Old Way (String Concatenation):**

```rustscript
let name = "Alice";
let age = 30;
let message = "Hello, " + name + "! You are " + age + " years old.";
```

Problems:
- Hard to read with all the `+` operators
- Easy to forget spaces
- Verbose and error-prone

**The RustScript Way (String Interpolation):**

```rustscript
let name = "Alice";
let age = 30;
let message = "Hello, {name}! You are {age} years old.";
```

**How it works:**
- Put expressions inside `{` and `}`
- The expression is evaluated and converted to a string
- Much more readable!

**Complex expressions:**

```rustscript
fn main() {
    let price = 100.0;
    let tax_rate = 0.13;
    
    let message = "Price: ${price}, Tax: ${price * tax_rate}, Total: ${price * (1.0 + tax_rate)}";
    console.log(message);
}
```

**Output:**
```
Price: $100, Tax: $13, Total: $113
```

**Why this matters:**

Compare these two approaches for a complex message:

```rustscript
// Without interpolation - hard to read
let msg = "User " + user.name + " (ID: " + user.id + ") logged in at " + timestamp.format() + " from " + user.location;

// With interpolation - crystal clear
let msg = "User {user.name} (ID: {user.id}) logged in at {timestamp.format()} from {user.location}";
```

### Optional Chaining: Safe Navigation

**The Problem:**

```rustscript
// What if user is null? What if address is null?
let city = user.address.city;  // Might crash!
```

**The Old Solution:**

```rustscript
let city;
if user != null {
    if user.address != null {
        city = user.address.city;
    } else {
        city = null;
    }
} else {
    city = null;
}
```

This is verbose and error-prone!

**The RustScript Way:**

```rustscript
let city = user?.address?.city;
```

**How it works:**
- `?.` checks if the value is null before accessing the property
- If any part is null, the whole expression becomes null
- No crashes, no verbose checks!

**Real-world example:**

```rustscript
struct Address {
    street: string,
    city: string,
    postal_code: string
}

struct User {
    name: string,
    address: Option<Address>
}

fn main() {
    let user = User {
        name: "Alice",
        address: Some(Address {
            street: "123 Main St",
            city: "Toronto",
            postal_code: "M5H 2N2"
        })
    };
    
    // Safe navigation through optional fields
    let city = user?.address?.city;
    console.log("City: {city}");
    
    // If address was None, city would be None
    let user_without_address = User {
        name: "Bob",
        address: None
    };
    
    let city2 = user_without_address?.address?.city;
    console.log("City: {city2}");  // Prints: City: None
}
```

### Null Coalescing: Default Values

**The Problem:**

```rustscript
// What if username is null?
let display_name;
if username != null {
    display_name = username;
} else {
    display_name = "Guest";
}
```

**The RustScript Way:**

```rustscript
let display_name = username ?? "Guest";
```

**How it works:**
- `??` checks if the left side is null
- If null, use the right side (the default)
- If not null, use the left side

**Chaining multiple fallbacks:**

```rustscript
fn main() {
    let user_preference = get_user_setting();
    let system_default = get_system_default();
    let hardcoded_default = "en-US";
    
    // Try user preference, then system default, then hardcoded
    let language = user_preference ?? system_default ?? hardcoded_default;
    
    console.log("Using language: {language}");
}
```

**Combining with optional chaining:**

```rustscript
fn main() {
    let user = get_current_user();
    
    // Get user's city, or "Unknown" if any part is missing
    let city = user?.address?.city ?? "Unknown";
    
    console.log("User's city: {city}");
}
```

### List Comprehensions: Transforming Collections

**The Old Way:**

```rustscript
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let mut evens = [];

for num in numbers {
    if num % 2 == 0 {
        evens.push(num * 2);
    }
}
```

**The RustScript Way:**

```rustscript
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let evens = [num * 2 for num in numbers if num % 2 == 0];
```

**How to read it:**
- `num * 2` - what to compute for each element
- `for num in numbers` - iterate over numbers
- `if num % 2 == 0` - only include even numbers

**More examples:**

```rustscript
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    
    // Square all numbers
    let squares = [n * n for n in numbers];
    console.log("Squares: {squares}");
    
    // Get only positive numbers
    let mixed = [-2, -1, 0, 1, 2];
    let positive = [n for n in mixed if n > 0];
    console.log("Positive: {positive}");
    
    // Transform strings
    let names = ["alice", "bob", "charlie"];
    let uppercase = [name.uppercase() for name in names];
    console.log("Uppercase: {uppercase}");
}
```

**Why this matters:**

List comprehensions are:
1. **More Readable**: Reads like English - "get n squared for n in numbers"
2. **More Concise**: One line instead of 4-5
3. **Less Error-Prone**: No manual array management
4. **Functional Style**: Encourages thinking about transformations

---

