# RustScript Tutorial: From Zero to Hero

**Author**: Michael Lauzon

Welcome to RustScript! This comprehensive tutorial will transform you from a complete beginner into a confident RustScript developer. We'll start with the absolute basics and progressively introduce advanced concepts, explaining not just *what* to do, but *why* it matters and *how* it works in practice.

RustScript combines the best ideas from 60+ programming languages spanning 68 years of computer science (1958-2025), creating a modern language that's both powerful and approachable.

## What You'll Learn

By the end of this tutorial, you'll master:

**Core Concepts**
- Variables, functions, control flow, and data structures
- Pattern matching and error handling the RustScript way
- Why immutability by default prevents entire classes of bugs

**Phase 1: Modern Syntax** (4 features)
- String interpolation for readable code
- Optional chaining for safe navigation
- Null coalescing for default values
- List comprehensions for data transformation

**Phase 2: Advanced Functions** (3 features)
- Pattern matching in function heads (Erlang-style)
- Generators for lazy evaluation (Python-style)
- Multiple dispatch for type-based polymorphism (Julia-style)

**Phase 3: Safety & Metaprogramming** (3 features)
- Design by Contract for formal verification
- Effect systems for tracking side effects
- Compile-time execution for zero-cost abstractions

**Phase 4: Production-Ready Features** (72 features)
- Memory safety without garbage collection (lifetimes, borrowing)
- Advanced type system (GADTs, higher-kinded types, refinement types)
- Functional programming (partial application, lenses, transducers)
- Safe concurrency (async/await, channels, atomic operations)
- Advanced control flow (try blocks, guard clauses, defer)
- Metaprogramming (macros, reflection, code generation)
- Domain-specific features (regex literals, operator overloading)

**Real-World Applications**
- Building web API clients
- Processing data pipelines
- Creating type-safe domain models
- Handling errors gracefully

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
12. [Phase 4: Advanced Language Features](#phase-4-advanced-language-features)
13. [Building Real Applications](#building-real-applications)

---

## Getting Started

### Installing RustScript

First, let's get RustScript installed on your system. Open your terminal and run:

**On Windows (PowerShell or Command Prompt):**
```powershell
git clone https://github.com/RustScript2025/RustScript.git
cd RustScript
cargo build --release
```

**On Linux/Mac:**
```bash
git clone https://github.com/RustScript2025/RustScript.git
cd RustScript
cargo build --release
```

This will take a few minutes as Cargo downloads dependencies and compiles the compiler. When it's done, you'll find the `rsxe` compiler at:
- **Windows**: `target\release\rsxe.exe`
- **Linux/Mac**: `target/release/rsxe`

**What just happened?**
- `cargo build` compiles the RustScript compiler (which is written in Rust)
- `--release` creates an optimised version for better performance
- The compiler is now ready to transform your `.rscc` files into JavaScript or WebAssembly

### Setting Up Your First Project

Create a new directory for your RustScript projects:

**On Windows (PowerShell):**
```powershell
mkdir rustscript-learning
cd rustscript-learning
```

**On Windows (Command Prompt):**
```cmd
mkdir rustscript-learning
cd rustscript-learning
```

**On Linux/Mac:**
```bash
mkdir rustscript-learning
cd rustscript-learning
```

Now you're ready to write your first programme!

---

## Hello, World! - Your First Programme

Every programming journey begins with "Hello, World!" - a simple programme that displays text. This tradition dates back to Brian Kernighan's 1972 tutorial and helps verify your setup works correctly.

### Writing the Programme

Create a file called `hello.rscc` (RustScript Code) and type:

```rustscript
fn main() {
    console.log("Hello, World!");
}

main();
```

### Understanding Each Line

Let's break down what each part means and *why* it's designed this way:

**Line 1: `fn main() {`**
- `fn` declares a function (inspired by Rust, ML, and other functional languages)
- `main` is the function name - by convention, the entry point of your programme
- `()` means no parameters - this function doesn't need any input
- `{` starts the function body - everything between `{` and `}` is part of this function

**Why functions?** Functions are the fundamental building blocks of RustScript. They let you organise code into reusable, testable pieces.

**Line 2: `console.log("Hello, World!");`**
- `console.log()` prints text to the console (borrowed from JavaScript)
- `"Hello, World!"` is a string literal - text enclosed in quotes
- `;` ends the statement (required in RustScript, like Rust and C)

**Why console.log?** RustScript compiles to JavaScript, so it can use JavaScript's built-in functions. This makes it easy to integrate with existing JavaScript code.

**Line 3: `}`**
- Closes the function body

**Line 5: `main();`**
- Actually *calls* (executes) the function we defined
- Without this line, we'd define the function but never run it
- `()` means we're passing no arguments

**Why explicit function calls?** Unlike some languages that automatically run `main()`, RustScript requires you to call it explicitly. This gives you control over when and how your code executes.

### Running Your Programme

Now let's compile and run it:

**On Windows:**
```powershell
..\target\release\rsxe.exe hello.rscc
node hello.js
```

**On Linux/Mac:**
```bash
../target/release/rsxe hello.rscc
node hello.js
```

You should see:
```
Hello, World!
```

**What happened behind the scenes?**
1. The RustScript compiler (`rsxe`) read your `.rscc` file
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

**On Windows:**
```powershell
python ..\serve.py
```

**On Linux/Mac:**
```bash
python3 ../serve.py
```

The development server includes many useful features like hot reload, HTTPS, file uploads, and more. For complete documentation, see [SERVE.md](SERVE.md).

Then open `http://localhost:8000/hello.html` in your browser and press F12 to see the console output.

**Why two targets (JavaScript and WebAssembly)?**
- **JavaScript**: Great for Node.js servers, command-line tools, and quick prototyping
- **WebAssembly**: Faster execution in browsers, better for performance-critical web apps

### Troubleshooting

#### Compiler Hangs or Takes Too Long?

If the compiler seems to hang when you run `rsxe hello.rscc`, this is likely because it's searching through a large directory tree. This can happen if you're in a directory with many subdirectories (like a git repository with `.git` folder, or a project with `node_modules`).

**Solution**: Make sure you're compiling a specific file, not a directory:

```bash
# ✅ Good - compiles just the file you specify
rsxe hello.rscc

# ❌ Avoid - searches entire directory tree
rsxe .
```

If you need to compile multiple files, put them in a dedicated directory without large subdirectories.

#### File Extension Errors?

Make sure your files use the `.rscc` extension (RustScript Code):

```bash
# ✅ Correct
rsxe hello.rscc

# ❌ Wrong extension
rsxe hello.rjsc  # Old extension, no longer supported
rsxe hello.js    # This is JavaScript, not RustScript
```

#### Node.js Not Found?

If you get "node: command not found" (or "'node' is not recognized" on Windows), you need to install Node.js:
- **Windows**: Download from [nodejs.org](https://nodejs.org/) or use `winget install OpenJS.NodeJS`
- **Mac**: Download from [nodejs.org](https://nodejs.org/) or use `brew install node`
- **Linux**: Use your package manager: `apt install nodejs` (Debian/Ubuntu) or `dnf install nodejs` (Fedora)

#### Python Server Issues?

If the serve command doesn't work:
- **Windows**: Try `python ..\serve.py` or `py ..\serve.py`
- **Linux/Mac**: Try `python3 ../serve.py` instead of `python`
- Make sure you're in the correct directory
- Check that the `www` directory exists:
  - **Windows**: Run `build_wasm.bat` first
  - **Linux/Mac**: Run `./build_wasm.sh` first
- See [SERVE.md](SERVE.md) for advanced options and troubleshooting

---

## Understanding Variables and Immutability

Variables store data that your programme can use and manipulate. RustScript has a unique approach to variables that prevents many common bugs.

### Immutable Variables: The Default

In RustScript, variables are **immutable by default** (inspired by Rust, Haskell, and OCaml). Once you assign a value, you can't change it:

```rustscript
fn main() {
    let name = "Alice";
    console.log("Hello, {name}!");
    
    // This would cause a compile error:
    // name = "Bob";  // ERROR: Cannot assign to immutable variable
}
```

**Why immutable by default?**

This might seem restrictive at first, but it's one of RustScript's most powerful safety features:

1. **Prevents Accidental Changes**: You can't accidentally modify a value somewhere deep in your code
2. **Makes Code Easier to Reason About**: When you see `let x = 5`, you *know* `x` will always be 5
3. **Enables Compiler Optimisations**: The compiler can make aggressive optimisations knowing values don't change
4. **Reduces Bugs**: Studies show 40-60% of bugs involve unexpected state changes
5. **Thread Safety**: Immutable data can be safely shared between threads without locks

**Real-world example - Shopping Cart:**
```rustscript
fn calculate_total(price: number, tax_rate: number, discount: number) -> number {
    let subtotal = price;
    let tax = subtotal * tax_rate;
    let discount_amount = subtotal * discount;
    let total = subtotal + tax - discount_amount;
    
    // Later in the function, you KNOW these values haven't changed
    // No defensive programming needed - the compiler guarantees it
    // This makes debugging much easier!
    
    console.log("Subtotal: ${subtotal}");  // Always the original price
    console.log("Tax: ${tax}");            // Always subtotal * tax_rate
    console.log("Discount: ${discount_amount}");
    console.log("Total: ${total}");
    
    total
}

fn main() {
    let final_price = calculate_total(100.0, 0.13, 0.10);
    console.log("You pay: ${final_price}");
}
```

**Output:**
```
Subtotal: $100
Tax: $13
Discount: $10
Total: $103
You pay: $103
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
- Counters and accumulators in loops
- Building up data structures incrementally
- Iterative algorithms (like sorting)
- Game state that changes over time
- Any time you need to update a value

**Real-world example - Processing a List:**
```rustscript
fn process_orders(orders: [Order]) -> OrderSummary {
    let mut total_revenue = 0.0;
    let mut total_items = 0;
    let mut failed_orders = 0;
    
    for order in orders {
        if order.status == "completed" {
            total_revenue += order.amount;
            total_items += order.items.length;
        } else {
            failed_orders += 1;
        }
    }
    
    OrderSummary {
        revenue: total_revenue,
        items: total_items,
        failed: failed_orders,
    }
}
```

**Best practice:** Start with immutable (`let`) and only add `mut` when you need it. This makes your intent crystal clear: "This value will change." It's like putting a warning sign on the variable.

**The Rule of Thumb:**
- If a value never changes → use `let` (immutable)
- If a value changes → use `let mut` (mutable)
- When in doubt → start with `let` and let the compiler tell you if you need `mut`

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

Functions can give back (return) a value to their caller:

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

**Output:**
```
5 + 3 = 8
10 + 20 = 30
```

**Key points about returns:**

1. **Return Type**: `-> number` declares what type this function returns
2. **Implicit Return**: The last expression (without `;`) is automatically returned
3. **No `return` Keyword Needed**: `a + b` is returned automatically
4. **Type Safety**: The compiler verifies you return the correct type

**Why implicit returns?**

This is inspired by functional programming languages (Rust, Scala, Ruby). It encourages thinking of functions as *expressions that evaluate to values*, rather than *procedures that execute steps*.

Compare these equivalent functions:

```rustscript
// RustScript style (implicit return) - preferred
fn multiply(a: number, b: number) -> number {
    a * b
}

// Traditional style (explicit return) - also works
fn multiply_explicit(a: number, b: number) -> number {
    return a * b;
}
```

Both work, but the first is more concise and idiomatic. Use explicit `return` when you need to return early:

```rustscript
fn divide(a: number, b: number) -> number {
    if b == 0.0 {
        console.error("Cannot divide by zero!");
        return 0.0;  // Early return - exit immediately
    }
    
    a / b  // Implicit return for normal case
}

fn main() {
    console.log("10 / 2 = {divide(10.0, 2.0)}");   // 5.0
    console.log("10 / 0 = {divide(10.0, 0.0)}");   // 0.0 (with error message)
}
```

**Real-world example - Calculating Discounts:**
```rustscript
fn calculate_discount(price: number, customer_type: string) -> number {
    if price < 0.0 {
        return 0.0;  // Invalid price, no discount
    }
    
    if customer_type == "premium" {
        return price * 0.20;  // 20% discount
    }
    
    if customer_type == "regular" {
        return price * 0.10;  // 10% discount
    }
    
    0.0  // No discount for other customers
}

fn main() {
    let price = 100.0;
    
    let premium_discount = calculate_discount(price, "premium");
    console.log("Premium customer saves: ${premium_discount}");
    
    let regular_discount = calculate_discount(price, "regular");
    console.log("Regular customer saves: ${regular_discount}");
}
```

**Output:**
```
Premium customer saves: $20
Regular customer saves: $10
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

Match ranges of values (inspired by Rust, Swift):

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

**Output:**
```
Age 10: child
Age 16: teenager
Age 30: adult
Age 70: senior
```

**Understanding range syntax:**
- `0..5` means 0, 1, 2, 3, 4 (excludes 5) - half-open range
- `0..=5` means 0, 1, 2, 3, 4, 5 (includes 5) - closed range
- Use `..=` in match expressions for inclusive ranges

**Real-world example - Grading System:**
```rustscript
fn get_letter_grade(score: number) -> string {
    match score {
        90..=100 => "A",
        80..=89 => "B",
        70..=79 => "C",
        60..=69 => "D",
        0..=59 => "F",
        _ => "Invalid score"
    }
}

fn get_feedback(score: number) -> string {
    match score {
        95..=100 => "Outstanding! Perfect work!",
        90..=94 => "Excellent work!",
        80..=89 => "Good job!",
        70..=79 => "Satisfactory",
        60..=69 => "Needs improvement",
        _ => "Please see instructor"
    }
}

fn main() {
    let scores = [95, 87, 72, 58];
    
    for score in scores {
        let grade = get_letter_grade(score);
        let feedback = get_feedback(score);
        console.log("Score {score}: Grade {grade} - {feedback}");
    }
}
```

**Output:**
```
Score 95: Grade A - Outstanding! Perfect work!
Score 87: Grade B - Good job!
Score 72: Grade C - Satisfactory
Score 58: Grade F - Please see instructor
```

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

**Inspired by**: Swift, Kotlin, Ruby, JavaScript template literals

**The Old Way (String Concatenation):**

```rustscript
let name = "Alice";
let age = 30;
let city = "Toronto";
let message = "Hello, " + name + "! You are " + age + " years old and live in " + city + ".";
```

Problems with concatenation:
- Hard to read with all the `+` operators
- Easy to forget spaces between parts
- Verbose and error-prone
- Difficult to maintain when adding more variables

**The RustScript Way (String Interpolation):**

```rustscript
let name = "Alice";
let age = 30;
let city = "Toronto";
let message = "Hello, {name}! You are {age} years old and live in {city}.";
```

**How it works:**
- Put expressions inside `{` and `}`
- The expression is evaluated and converted to a string
- Much more readable and maintainable!

**Complex expressions work too:**

```rustscript
fn main() {
    let price = 100.0;
    let tax_rate = 0.13;
    let quantity = 3;
    
    let subtotal = price * quantity;
    let tax = subtotal * tax_rate;
    let total = subtotal + tax;
    
    let receipt = "
        Items: {quantity}
        Price each: ${price}
        Subtotal: ${subtotal}
        Tax ({tax_rate * 100}%): ${tax}
        Total: ${total}
    ";
    
    console.log(receipt);
}
```

**Output:**
```
Items: 3
Price each: $100
Subtotal: $300
Tax (13%): $39
Total: $339
```

**Real-world example - Logging:**

```rustscript
struct User {
    id: number,
    name: string,
    email: string,
}

fn log_user_action(user: User, action: string, timestamp: number) {
    // Without interpolation - hard to read
    let old_way = "User " + user.name + " (ID: " + user.id + ", Email: " + user.email + ") performed action: " + action + " at " + timestamp;
    
    // With interpolation - crystal clear!
    let new_way = "User {user.name} (ID: {user.id}, Email: {user.email}) performed action: {action} at {timestamp}";
    
    console.log(new_way);
}

fn main() {
    let user = User {
        id: 12345,
        name: "Alice Johnson",
        email: "alice@example.com",
    };
    
    log_user_action(user, "login", 1638360000);
    log_user_action(user, "update_profile", 1638360120);
    log_user_action(user, "logout", 1638363600);
}
```

**Output:**
```
User Alice Johnson (ID: 12345, Email: alice@example.com) performed action: login at 1638360000
User Alice Johnson (ID: 12345, Email: alice@example.com) performed action: update_profile at 1638360120
User Alice Johnson (ID: 12345, Email: alice@example.com) performed action: logout at 1638363600
```

**Why this matters:**
- **Readability**: Code reads like natural language
- **Maintainability**: Easy to add or remove variables
- **Fewer Bugs**: No missing spaces or concatenation errors
- **Performance**: Compiler can optimise string building

### Optional Chaining: Safe Navigation

**Inspired by**: Swift, C#, TypeScript, Kotlin

**The Problem:**

```rustscript
// What if user is null? What if address is null?
let city = user.address.city;  // Might crash with null pointer error!
```

**The Old Solution (Defensive Programming):**

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

This is:
- Verbose (6 lines for one simple operation!)
- Error-prone (easy to forget a check)
- Hard to read (deeply nested)
- Tedious to write

**The RustScript Way:**

```rustscript
let city = user?.address?.city;
```

**How it works:**
- `?.` checks if the value is null/None before accessing the property
- If any part is null, the whole expression short-circuits and returns null
- No crashes, no verbose checks, no nested ifs!

**Real-world example - E-commerce System:**

```rustscript
struct Address {
    street: string,
    city: string,
    postal_code: string,
    country: string,
}

struct ShippingInfo {
    address: Option<Address>,
    phone: Option<string>,
}

struct User {
    name: string,
    email: string,
    shipping: Option<ShippingInfo>,
}

fn get_shipping_city(user: User) -> Option<string> {
    // Without optional chaining - nightmare!
    if user.shipping != null {
        if user.shipping.address != null {
            return Some(user.shipping.address.city);
        }
    }
    None
    
    // With optional chaining - elegant!
    user?.shipping?.address?.city
}

fn display_user_info(user: User) {
    console.log("User: {user.name}");
    console.log("Email: {user.email}");
    
    // Safe navigation through potentially missing data
    let city = user?.shipping?.address?.city ?? "Not provided";
    let country = user?.shipping?.address?.country ?? "Not provided";
    let phone = user?.shipping?.phone ?? "Not provided";
    
    console.log("Ships to: {city}, {country}");
    console.log("Contact: {phone}");
}

fn main() {
    // User with complete shipping info
    let alice = User {
        name: "Alice",
        email: "alice@example.com",
        shipping: Some(ShippingInfo {
            address: Some(Address {
                street: "123 Main St",
                city: "Toronto",
                postal_code: "M5H 2N2",
                country: "Canada",
            }),
            phone: Some("555-1234"),
        }),
    };
    
    // User with no shipping info
    let bob = User {
        name: "Bob",
        email: "bob@example.com",
        shipping: None,
    };
    
    // User with partial shipping info
    let charlie = User {
        name: "Charlie",
        email: "charlie@example.com",
        shipping: Some(ShippingInfo {
            address: None,
            phone: Some("555-5678"),
        }),
    };
    
    display_user_info(alice);
    console.log("");
    display_user_info(bob);
    console.log("");
    display_user_info(charlie);
}
```

**Output:**
```
User: Alice
Email: alice@example.com
Ships to: Toronto, Canada
Contact: 555-1234

User: Bob
Email: bob@example.com
Ships to: Not provided, Not provided
Contact: Not provided

User: Charlie
Email: charlie@example.com
Ships to: Not provided, Not provided
Contact: 555-5678
```

**Why this matters:**
- **Safety**: No null pointer crashes
- **Conciseness**: One line instead of many
- **Readability**: Intent is crystal clear
- **Maintainability**: Easy to add more levels of nesting

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

**Inspired by**: Python, Haskell, Scala, F#

**The Old Way (Imperative Style):**

```rustscript
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let mut evens_doubled = [];

for num in numbers {
    if num % 2 == 0 {
        evens_doubled.push(num * 2);
    }
}
```

Problems:
- Verbose (5 lines for a simple operation)
- Requires mutable variable
- Intent is buried in implementation details
- Easy to make mistakes (forget to push, wrong condition, etc.)

**The RustScript Way (Declarative Style):**

```rustscript
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let evens_doubled = [num * 2 for num in numbers if num % 2 == 0];
```

**How to read it (left to right):**
1. `num * 2` - what to compute for each element (the transformation)
2. `for num in numbers` - iterate over the numbers collection
3. `if num % 2 == 0` - only include even numbers (the filter)

Result: `[4, 8, 12, 16, 20]`

**More examples:**

```rustscript
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    
    // Square all numbers
    let squares = [n * n for n in numbers];
    console.log("Squares: {squares}");  // [1, 4, 9, 16, 25]
    
    // Get only positive numbers
    let mixed = [-2, -1, 0, 1, 2];
    let positive = [n for n in mixed if n > 0];
    console.log("Positive: {positive}");  // [1, 2]
    
    // Transform strings
    let names = ["alice", "bob", "charlie"];
    let uppercase = [name.uppercase() for name in names];
    console.log("Uppercase: {uppercase}");  // ["ALICE", "BOB", "CHARLIE"]
    
    // Complex transformation
    let prices = [10.0, 25.0, 50.0, 100.0];
    let discounted = [price * 0.8 for price in prices if price > 20.0];
    console.log("Discounted (20% off items over $20): {discounted}");  // [20.0, 40.0, 80.0]
}
```

**Real-world example - Data Processing:**

```rustscript
struct Product {
    name: string,
    price: number,
    in_stock: bool,
    category: string,
}

fn main() {
    let products = [
        Product { name: "Laptop", price: 1200.0, in_stock: true, category: "Electronics" },
        Product { name: "Mouse", price: 25.0, in_stock: true, category: "Electronics" },
        Product { name: "Desk", price: 300.0, in_stock: false, category: "Furniture" },
        Product { name: "Chair", price: 150.0, in_stock: true, category: "Furniture" },
        Product { name: "Monitor", price: 400.0, in_stock: true, category: "Electronics" },
    ];
    
    // Get names of available electronics
    let available_electronics = [
        p.name 
        for p in products 
        if p.in_stock && p.category == "Electronics"
    ];
    console.log("Available electronics: {available_electronics}");
    // ["Laptop", "Mouse", "Monitor"]
    
    // Calculate discounted prices for expensive items
    let expensive_discounted = [
        (p.name, p.price * 0.9)
        for p in products
        if p.price > 100.0 && p.in_stock
    ];
    console.log("Expensive items with 10% discount:");
    for (name, price) in expensive_discounted {
        console.log("  {name}: ${price}");
    }
    // Laptop: $1080
    // Chair: $135
    // Monitor: $360
    
    // Get all unique categories
    let categories = [p.category for p in products];
    console.log("Categories: {categories}");
    // ["Electronics", "Electronics", "Furniture", "Furniture", "Electronics"]
}
```

**Nested comprehensions (advanced):**

```rustscript
fn main() {
    // Generate all coordinate pairs
    let coords = [
        (x, y) 
        for x in [1, 2, 3] 
        for y in [4, 5, 6]
    ];
    console.log("Coordinates: {coords}");
    // [(1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)]
    
    // Multiplication table
    let mult_table = [
        (x, y, x * y)
        for x in [1, 2, 3, 4, 5]
        for y in [1, 2, 3, 4, 5]
    ];
    
    for (x, y, product) in mult_table {
        console.log("{x} × {y} = {product}");
    }
}
```

**Why this matters:**

List comprehensions are:
1. **More Readable**: Reads like English - "get n squared for n in numbers where n is positive"
2. **More Concise**: One line instead of 4-5 lines of imperative code
3. **Less Error-Prone**: No manual array management, no mutable state
4. **Functional Style**: Encourages thinking about *what* you want, not *how* to get it
5. **Composable**: Easy to chain transformations
6. **Optimisable**: Compiler can optimise better than hand-written loops

**When to use list comprehensions:**
- ✅ Transforming data (mapping)
- ✅ Filtering collections
- ✅ Combining map + filter operations
- ✅ Generating new collections from existing ones
- ❌ Complex multi-step algorithms (use regular loops)
- ❌ Side effects (use regular loops with for)

---



## Phase 2: Advanced Function Capabilities

Phase 2 adds powerful function features inspired by functional programming languages.

### Pattern Matching in Function Heads

**Inspired by**: Erlang, Elixir, Haskell, ML

Instead of one function with if-else logic, define multiple versions that match different patterns:

```rustscript
// Traditional approach
fn factorial(n: number) -> number {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Pattern matching approach
fn factorial(0) -> number { 1 }
fn factorial(n) -> number { n * factorial(n - 1) }
```

**Why this is better:**
- Each case is clearly separated
- No nested if-else statements
- Reads like mathematical definitions
- Compiler ensures all cases are handled

**More examples:**

```rustscript
// List processing
fn sum([]) -> number { 0 }
fn sum([head, ...tail]) -> number { head + sum(tail) }

// String processing
fn greet("") -> string { "Hello, stranger!" }
fn greet(name) -> string { "Hello, {name}!" }
```

### Generators

**Inspired by**: Python, JavaScript ES6, C#, Icon

Generators produce values lazily, one at a time:

```rustscript
gen fn fibonacci() {
    let (a, b) = (0, 1);
    loop {
        yield a;
        (a, b) = (b, a + b);
    }
}

fn main() {
    // Take first 10 Fibonacci numbers
    for fib in take(10, fibonacci()) {
        console.log(fib);
    }
}
```

**Why generators matter:**

1. **Memory Efficient**: Don't need to store entire sequence
2. **Infinite Sequences**: Can represent infinite data
3. **Lazy Evaluation**: Only compute what you need
4. **Clean Code**: Separate generation logic from consumption

**Practical example:**

```rustscript
gen fn read_lines(file: string) {
    let handle = open(file);
    loop {
        match read_line(handle) {
            Some(line) => yield line,
            None => break
        }
    }
    close(handle);
}

// Process file line by line without loading entire file
for line in read_lines("large_file.txt") {
    process(line);
}
```

### Multiple Dispatch

**Inspired by**: Julia, Common Lisp CLOS, Dylan, Clojure

Functions can have different implementations based on the types of ALL arguments:

```rustscript
// Different behaviour for different type combinations
fn process(x: number, y: number) -> string {
    "Adding numbers: {x + y}"
}

fn process(x: string, y: string) -> string {
    "Concatenating strings: {x}{y}"
}

fn process(x: number, y: string) -> string {
    "Mixed types: {x} and {y}"
}

fn main() {
    console.log(process(5, 10));           // "Adding numbers: 15"
    console.log(process("Hello", "World")); // "Concatenating strings: HelloWorld"
    console.log(process(42, "answer"));     // "Mixed types: 42 and answer"
}
```

**Why multiple dispatch matters:**

- **Symmetric**: All arguments participate in dispatch, not just the first
- **Extensible**: Add new type combinations without modifying existing code
- **Natural**: Matches how we think about operations (addition works on numbers, strings, matrices, etc.)

---

## Phase 3: Safety and Metaprogramming

Phase 3 adds formal verification and compile-time programming capabilities.

### Design by Contract

**Inspired by**: Eiffel, D, Ada, Spec#

Specify preconditions, postconditions, and invariants:

```rustscript
fn withdraw(balance: number, amount: number) -> number
    requires amount > 0, "Amount must be positive"
    requires balance >= amount, "Insufficient funds"
    ensures result >= 0, "Balance cannot be negative"
    ensures result == balance - amount, "Correct subtraction"
{
    balance - amount
}
```

**Why contracts matter:**

1. **Documentation**: Contracts are executable documentation
2. **Verification**: Checked at compile time when possible
3. **Debugging**: Clear error messages when contracts fail
4. **Correctness**: Formal specification of behaviour

**Invariants for data structures:**

```rustscript
struct BankAccount {
    balance: number,
    overdraft_limit: number
}

impl BankAccount {
    invariant self.balance >= -self.overdraft_limit, "Balance within limits"
    
    fn withdraw(&mut self, amount: number)
        requires amount > 0
        ensures self.balance >= -self.overdraft_limit
    {
        self.balance -= amount;
    }
}
```

### Effect System

**Inspired by**: Koka, Eff, Nim, Rust traits

Track side effects in function signatures:

```rustscript
// Pure function - no side effects
effect [pure]
fn add(a: number, b: number) -> number {
    a + b
}

// Function with I/O
effect [io]
fn read_file(path: string) -> string {
    // Can perform I/O operations
}

// Function with multiple effects
effect [io, state, throws]
fn process_data(file: string) -> Result<Data, Error> {
    // Can do I/O, modify state, and throw errors
}
```

**Why effect systems matter:**

- **Reasoning**: Know what a function can do by looking at its signature
- **Safety**: Prevent accidental side effects in pure code
- **Optimisation**: Compiler can optimise pure functions more aggressively
- **Testing**: Pure functions are easier to test

### Compile-time Execution

**Inspired by**: Zig, D CTFE, C++ constexpr, Nim

Run code at compile time:

```rustscript
comptime {
    // This runs during compilation
    const BUFFER_SIZE = 1024;
    
    fn fibonacci(n: number) -> number {
        if n <= 1 { n }
        else { fibonacci(n - 1) + fibonacci(n - 2) }
    }
    
    const FIB_10 = fibonacci(10);  // Computed at compile time
}

fn main() {
    // FIB_10 is a compile-time constant, no runtime computation
    console.log("Fibonacci(10) = {FIB_10}");
}
```

**Why compile-time execution matters:**

1. **Performance**: Move work from runtime to compile time
2. **Safety**: Catch errors during compilation
3. **Code Generation**: Generate code based on compile-time information
4. **Zero Cost**: No runtime overhead

---

## Phase 4: Advanced Language Features

Phase 4 represents a massive leap forward, adding 62 advanced features that bring RustScript to feature parity with modern systems programming languages. These features span memory safety, sophisticated type systems, functional programming, safe concurrency, advanced control flow, metaprogramming, and domain-specific capabilities.

### Why Phase 4 Matters

Before Phase 4, RustScript was a modern scripting language with great syntax. After Phase 4, it's a full-featured systems programming language that can:
- Guarantee memory safety without garbage collection
- Catch complex bugs at compile time with advanced types
- Handle concurrency safely without data races
- Generate code at compile time for zero-cost abstractions
- Provide domain-specific features for common patterns

### Phase 4 Overview

**Phase 4A: Core Memory Safety (10 features)**  
Rust-level memory safety with lifetimes, borrowing, and move semantics.

**Phase 4B: Advanced Type System (15 features)**  
Sophisticated types including GADTs, higher-kinded types, and refinement types.

**Phase 4C: Functional Programming (10 features)**  
Pure functional features like partial application, lenses, and transducers.

**Phase 4D: Concurrency & Parallelism (8 features)**  
Safe concurrency with async/await, channels, and atomic operations.

**Phase 4E: Advanced Control Flow (10 features)**  
Explicit error handling with try blocks, guard clauses, and defer.

**Phase 4F: Metaprogramming & Macros (8 features)**  
Compile-time code generation with macros and reflection.

**Phase 4G: Domain-Specific Features (6 features)**  
Language-level support for regex, operator overloading, and destructuring.

**Phase 4H: Additional Utilities (5 features)**  
Developer experience improvements like default parameters and const functions.

---

### Phase 4A: Memory Safety Without Garbage Collection

#### Lifetimes: Preventing Dangling References

**Inspired by**: Rust, Cyclone

Lifetimes ensure references never outlive the data they point to:

```rustscript
// Lifetime 'a ensures the returned reference lives as long as the inputs
fn longest<'a>(x: &'a string, y: &'a string) -> &'a string {
    if x.length > y.length { x } else { y }
}

fn main() {
    let string1 = "Hello, world!";
    let string2 = "Rust";
    
    let result = longest(&string1, &string2);
    console.log("Longest: {result}");  // Safe - both strings still alive
}
```

**Why lifetimes matter**: They catch use-after-free bugs at compile time, not runtime. No null pointer crashes, no memory corruption.

#### Borrowing: Multiple Readers OR One Writer

**Inspired by**: Rust

The borrow checker enforces a simple rule: you can have either multiple immutable references OR one mutable reference, but not both:

```rustscript
struct Account {
    balance: number,
    name: string,
}

// Immutable borrow - can read but not modify
fn check_balance(account: &Account) -> number {
    account.balance
}

// Mutable borrow - can modify
fn deposit(account: &mut Account, amount: number) {
    account.balance += amount;
}

fn main() {
    let mut acc = Account { balance: 1000, name: "Alice" };
    
    // Multiple immutable borrows are fine
    let bal1 = check_balance(&acc);
    let bal2 = check_balance(&acc);
    console.log("Balance: {bal1}");
    
    // Mutable borrow (exclusive)
    deposit(&mut acc, 500);
    console.log("After deposit: {acc.balance}");
}
```

**Why borrowing matters**: Prevents data races at compile time. No locks needed, no race conditions possible.

#### Tail Call Optimisation: Infinite Recursion Without Stack Overflow

**Inspired by**: Scheme, Lua

Tail-recursive functions are automatically converted to loops:

```rustscript
// This looks recursive but compiles to a loop
fn factorial(n: number, acc: number) -> number {
    if n <= 1 {
        acc
    } else {
        factorial(n - 1, n * acc)  // Tail call - optimised to loop
    }
}

fn main() {
    // No stack overflow, even with huge numbers
    console.log("Factorial(100): {factorial(100, 1)}");
}
```

**Why TCO matters**: Write elegant recursive code without worrying about stack limits.

---

### Phase 4B: Sophisticated Type System

#### Union Types: Flexible Type Combinations

**Inspired by**: TypeScript, Ceylon

A value can be one of several types:

```rustscript
type StringOrNumber = string | number;

fn process(value: StringOrNumber) -> string {
    match value {
        string(s) => "Got string: {s}",
        number(n) => "Got number: {n}",
    }
}

fn main() {
    console.log(process("hello"));  // "Got string: hello"
    console.log(process(42));       // "Got number: 42"
}
```

#### Newtype Pattern: Zero-Cost Type Safety

**Inspired by**: Haskell, Rust

Prevent mixing incompatible values:

```rustscript
struct Metres(number);
struct Kilometres(number);
struct Seconds(number);

fn calculate_speed(distance: Metres, time: Seconds) -> number {
    distance.0 / time.0
}

fn main() {
    let dist = Metres(100);
    let time = Seconds(10);
    let speed = calculate_speed(dist, time);
    console.log("Speed: {speed} m/s");
    
    // This would be a compile error:
    // let km = Kilometres(5);
    // calculate_speed(km, time);  // ERROR: Expected Metres, got Kilometres
}
```

**Why newtypes matter**: Catch unit confusion bugs at compile time. No runtime overhead.

#### GADTs: Type-Safe Expression Trees

**Inspired by**: Haskell, OCaml

Generalised Algebraic Data Types allow different constructors to return different specialised types:

```rustscript
enum Expr<T> {
    IntLit(i32) -> Expr<i32>,
    BoolLit(bool) -> Expr<bool>,
    Add(Expr<i32>, Expr<i32>) -> Expr<i32>,
    Eq(Expr<i32>, Expr<i32>) -> Expr<bool>,
}

// Type-safe evaluation - return type matches expression type
fn eval_int(expr: Expr<i32>) -> i32 {
    match expr {
        Expr::IntLit(n) => n,
        Expr::Add(a, b) => eval_int(a) + eval_int(b),
        // Can't match BoolLit here - type system prevents it!
    }
}

fn eval_bool(expr: Expr<bool>) -> bool {
    match expr {
        Expr::BoolLit(b) => b,
        Expr::Eq(a, b) => eval_int(a) == eval_int(b),
    }
}
```

**Why GADTs matter**: Impossible to mix up expression types. Type safety for interpreters and compilers.

---

### Phase 4C: Pure Functional Programming

#### Partial Application: Fix Some Arguments

**Inspired by**: Haskell, ML, F#

Create new functions by fixing some arguments:

```rustscript
fn add(a: number, b: number) -> number {
    a + b
}

fn main() {
    // Fix first argument to 5
    let add5 = add(5, _);
    
    console.log(add5(10));  // 15
    console.log(add5(20));  // 25
    console.log(add5(30));  // 35
    
    // Fix second argument to 10
    let add_to_10 = add(_, 10);
    console.log(add_to_10(5));   // 15
    console.log(add_to_10(15));  // 25
}
```

**Real-world use case:**

```rustscript
fn log(level: string, module: string, message: string) {
    console.log("[{level}] [{module}] {message}");
}

fn main() {
    // Create specialised logging functions
    let log_error = log("ERROR", _, _);
    let log_auth_error = log("ERROR", "auth", _);
    
    log_auth_error("Login failed");
    log_auth_error("Invalid token");
    log_auth_error("Session expired");
}
```

#### Function Composition: Chain Operations

**Inspired by**: Haskell, F#, Elixir

Combine functions to create new functions:

```rustscript
fn add_one(x: number) -> number { x + 1 }
fn double(x: number) -> number { x * 2 }
fn square(x: number) -> number { x * x }

fn main() {
    // Forward composition: apply left to right
    let process = add_one >> double >> square;
    
    let result = process(5);  // ((5 + 1) * 2)^2 = 144
    console.log("Result: {result}");
    
    // Backward composition: apply right to left
    let process2 = square << double << add_one;
    let result2 = process2(5);  // Same as above
}
```

**Data transformation pipeline:**

```rustscript
fn trim(s: string) -> string { s.trim() }
fn to_lowercase(s: string) -> string { s.to_lowercase() }
fn remove_spaces(s: string) -> string { s.replace(" ", "") }

fn main() {
    let normalise = trim >> to_lowercase >> remove_spaces;
    
    let input = "  Hello World  ";
    let output = normalise(input);
    console.log("Normalised: '{output}'");  // "helloworld"
}
```

#### Currying: Single-Argument Function Chains

**Inspired by**: Haskell, ML, OCaml

Transform multi-parameter functions into chains of single-parameter functions:

```rustscript
// Curried function syntax
fn multiply(a: number)(b: number)(c: number) -> number {
    a * b * c
}

fn main() {
    // Partial application at each level
    let mul2 = multiply(2);
    let mul2_3 = mul2(3);
    let result = mul2_3(4);  // 2 * 3 * 4 = 24
    
    // Or call directly
    let result2 = multiply(5)(6)(7);  // 210
    
    console.log("Results: {result}, {result2}");
}
```

**Configuration with currying:**

```rustscript
fn create_logger(level: string)(module: string)(message: string) {
    console.log("[{level}] [{module}] {message}");
}

fn main() {
    // Build up specialised loggers
    let error_log = create_logger("ERROR");
    let auth_error = error_log("auth");
    
    auth_error("Login failed");
    auth_error("Invalid credentials");
    auth_error("Session timeout");
}
```

---

### Phase 4D: Safe Concurrency

#### Async/Await: Non-Blocking Operations

**Inspired by**: C#, JavaScript, Rust

Write asynchronous code that looks synchronous:

```rustscript
async fn fetch_user(id: number) -> Result<User, Error> {
    console.log("Fetching user {id}...");
    let response = await http.get("https://api.example.com/users/{id}")?;
    Ok(response.json())
}

async fn fetch_posts(user_id: number) -> Result<[Post], Error> {
    console.log("Fetching posts for user {user_id}...");
    let response = await http.get("https://api.example.com/posts?user={user_id}")?;
    Ok(response.json())
}

fn main() {
    // Run async code
    let result = block_on(async {
        let user = await fetch_user(1)?;
        console.log("User: {user.name}");
        
        let posts = await fetch_posts(user.id)?;
        console.log("Posts: {posts.length}");
        
        Ok(())
    });
    
    match result {
        Ok(_) => console.log("Success!"),
        Err(e) => console.error("Error: {e}")
    }
}
```

**Parallel async operations:**

```rustscript
async fn load_dashboard() -> Result<Dashboard, Error> {
    // Fetch multiple things in parallel
    let (user, posts, comments) = await Future::join3(
        fetch_user(1),
        fetch_posts(1),
        fetch_comments(1)
    )?;
    
    Ok(Dashboard { user, posts, comments })
}
```

#### Channels: Message Passing

**Inspired by**: Go, Rust

Communicate between concurrent tasks safely:

```rustscript
fn main() {
    let (tx, rx) = channel();
    
    // Spawn a task that sends messages
    spawn {
        for i in 0..5 {
            tx.send(i);
            console.log("Sent: {i}");
        }
    };
    
    // Receive messages
    for _ in 0..5 {
        let msg = rx.recv();
        console.log("Received: {msg}");
    }
}
```

**Multiple producers, single consumer:**

```rustscript
fn main() {
    let (tx, rx) = channel();
    
    // Spawn multiple producers
    for worker_id in 0..3 {
        let tx_clone = tx.clone();
        spawn {
            for i in 0..5 {
                tx_clone.send("Worker {worker_id}: {i}");
            }
        };
    }
    
    // Receive all messages
    for _ in 0..15 {
        console.log(rx.recv());
    }
}
```

---

### Phase 4E: Explicit Error Handling

#### Try Blocks: Multiple Error Types

**Inspired by**: Rust, Kotlin

Handle different error types elegantly:

```rustscript
enum FileError {
    NotFound(string),
    PermissionDenied(string),
}

enum ParseError {
    InvalidFormat(string),
    MissingField(string),
}

fn process_config() -> Result<Config, AppError> {
    try {
        let file = read_file("config.json")?;
        let data = parse_json(file)?;
        Ok(data)
    } catch FileError as e {
        console.error("File error: {e}");
        Err(AppError::File(e))
    } catch ParseError as e {
        console.error("Parse error: {e}");
        Err(AppError::Parse(e))
    }
}
```

#### Guard Clauses: Early Returns

**Inspired by**: Swift, Ruby

Reduce nesting with early returns:

```rustscript
// Without guards - nested and hard to read
fn process_order(order: Order) -> Result<(), Error> {
    if order.items.length > 0 {
        if order.total > 0 {
            if order.customer.verified {
                // Finally, the actual logic
                console.log("Processing order");
                Ok(())
            } else {
                Err(Error::NotVerified)
            }
        } else {
            Err(Error::InvalidTotal)
        }
    } else {
        Err(Error::EmptyOrder)
    }
}

// With guards - flat and clear
fn process_order_better(order: Order) -> Result<(), Error> {
    guard order.items.length > 0 else {
        return Err(Error::EmptyOrder);
    };
    
    guard order.total > 0 else {
        return Err(Error::InvalidTotal);
    };
    
    guard order.customer.verified else {
        return Err(Error::NotVerified);
    };
    
    // Happy path is prominent
    console.log("Processing order");
    Ok(())
}
```

#### Defer: Guaranteed Cleanup

**Inspired by**: Go, Swift, Zig

Ensure cleanup code runs no matter how a function exits:

```rustscript
fn process_file(path: string) -> Result<(), Error> {
    let file = open(path)?;
    
    defer {
        close(file);  // Always runs, even on error
        console.log("File closed");
    };
    
    let content = read(file)?;
    let processed = transform(content)?;
    write(file, processed)?;
    
    Ok(())
    // defer block runs here automatically
}
```

**Multiple defers execute in LIFO order:**

```rustscript
fn acquire_resources() {
    let resource1 = acquire("Resource 1");
    defer { release(resource1); console.log("Released 1"); };
    
    let resource2 = acquire("Resource 2");
    defer { release(resource2); console.log("Released 2"); };
    
    let resource3 = acquire("Resource 3");
    defer { release(resource3); console.log("Released 3"); };
    
    // Releases in order: 3, 2, 1 (LIFO)
}
```

---

### Phase 4F: Metaprogramming Power

#### Declarative Macros: Pattern-Based Code Generation

**Inspired by**: Rust macro_rules!, Lisp

Generate code with pattern matching:

```rustscript
macro_rules! vec {
    ($($x:expr),*) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

fn main() {
    let numbers = vec!(1, 2, 3, 4, 5);
    console.log("Vector: {numbers}");
}
```

#### Procedural Macros: Automatic Trait Implementation

**Inspired by**: Rust, Java annotations

Automatically generate boilerplate code:

```rustscript
#[derive(Debug, Clone, PartialEq)]
struct User {
    id: number,
    name: string,
    email: string,
}

fn main() {
    let user1 = User { id: 1, name: "Alice", email: "alice@example.com" };
    let user2 = user1.clone();  // Clone trait auto-implemented
    
    if user1 == user2 {  // PartialEq trait auto-implemented
        console.log("Users are equal");
    }
    
    console.log("User: {user1:?}");  // Debug trait auto-implemented
}
```

#### Compile-Time Reflection: Type Introspection

**Inspired by**: Zig, D

Inspect types at compile time:

```rustscript
struct Point {
    x: number,
    y: number,
    z: number,
}

comptime {
    let info = @typeInfo(Point);
    console.log("Type: {info.name}");
    console.log("Fields: {info.fields.length}");
    
    for field in info.fields {
        console.log("  {field.name}: {field.type}");
    }
}
```

---

### Phase 4G: Domain-Specific Features

#### Regex Literals: First-Class Pattern Matching

**Inspired by**: Perl, JavaScript, Ruby

Built-in regex support with compile-time validation:

```rustscript
fn main() {
    let email_pattern = r/[\w\.-]+@[\w\.-]+\.\w+/;
    let phone_pattern = r/\d{3}-\d{3}-\d{4}/g;
    
    let email = "user@example.com";
    if email_pattern.test(email) {
        console.log("Valid email");
    }
    
    let text = "Call me at 555-123-4567 or 555-987-6543";
    let phones = text.match(phone_pattern);
    console.log("Found phones: {phones}");
}
```

#### Operator Overloading: Custom Operators

**Inspired by**: C++, Rust, Python

Define how operators work for your types:

```rustscript
struct Vec2 {
    x: number,
    y: number,
}

impl Add for Vec2 {
    type Output = Vec2;
    
    fn add(self: Vec2, other: Vec2) -> Vec2 {
        Vec2 {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn main() {
    let v1 = Vec2 { x: 3.0, y: 4.0 };
    let v2 = Vec2 { x: 1.0, y: 2.0 };
    
    let sum = v1 + v2;  // Uses our custom Add implementation
    console.log("Sum: ({sum.x}, {sum.y})");
}
```

#### Destructuring: Pattern-Based Assignment

**Inspired by**: JavaScript ES6, Rust, Python

Extract values from complex structures:

```rustscript
fn main() {
    // Tuple destructuring
    let point = (10, 20);
    let (x, y) = point;
    console.log("x: {x}, y: {y}");
    
    // Struct destructuring
    struct User { name: string, age: number }
    let user = User { name: "Alice", age: 30 };
    let User { name, age } = user;
    console.log("{name} is {age} years old");
    
    // Nested destructuring
    let ((a, b), (c, d)) = ((1, 2), (3, 4));
    console.log("Values: {a}, {b}, {c}, {d}");
}
```

---

### Phase 4H: Developer Experience

#### Default Parameters: Sensible Defaults

**Inspired by**: Python, JavaScript, Rust

Provide default values for optional parameters:

```rustscript
fn greet(name: string = "World", formal: bool = false) {
    if formal {
        console.log("Good day, {name}!");
    } else {
        console.log("Hi, {name}!");
    }
}

fn main() {
    greet();                           // "Hi, World!"
    greet("Alice");                    // "Hi, Alice!"
    greet("Bob", true);                // "Good day, Bob!"
    greet(name: "Charlie", formal: true);  // Named arguments
}
```

#### Const Functions: Compile-Time Computation

**Inspired by**: Rust, C++ constexpr

Functions that can run at compile time:

```rustscript
const fn factorial(n: number) -> number {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
}

// Computed at compile time
const FACT_5 = factorial(5);
const FACT_10 = factorial(10);

fn main() {
    console.log("5! = {FACT_5}");    // No runtime computation
    console.log("10! = {FACT_10}");  // No runtime computation
    
    // Can also call at runtime
    let runtime_fact = factorial(7);
    console.log("7! = {runtime_fact}");
}
```

---

### Putting It All Together

Here's a real-world example using features from all phases:

```rustscript
// Phase 4B: Newtype for type safety
struct UserId(number);
struct Email(string);

// Phase 4F: Derive macros
#[derive(Debug, Clone)]
struct User {
    id: UserId,
    name: string,
    email: Email,
    age: number,
}

// Phase 3: Design by Contract
fn create_user(name: string, email: string, age: number) -> Result<User, Error>
    requires !name.is_empty(), "Name cannot be empty"
    requires age >= 0 && age <= 150, "Age must be realistic"
    effect [pure]
{
    Ok(User {
        id: UserId(generate_id()),
        name,
        email: Email(email),
        age,
    })
}

// Phase 4D: Async database operations
async fn save_user(user: &User) -> Result<(), Error>
    effect [io, throws]
{
    let query = "INSERT INTO users VALUES ({user.id.0}, '{user.name}', '{user.email.0}', {user.age})";
    await db.execute(query)?;
    Ok(())
}

// Phase 4A: Borrowing for efficiency
fn validate_email(email: &Email) -> bool {
    let pattern = r/[\w\.-]+@[\w\.-]+\.\w+/;  // Phase 4G: Regex literals
    pattern.test(email.0)
}

// Phase 4C: Function composition
let process_user = validate_email >> save_user >> notify_admin;

fn main() {
    // Phase 1: String interpolation, optional chaining, null coalescing
    let name = user_input?.name ?? "Anonymous";
    let email = user_input?.email ?? "no-email@example.com";
    let age = user_input?.age ?? 0;
    
    // Phase 2: Pattern matching in function heads
    match create_user(name, email, age) {
        Ok(user) => {
            console.log("Created user: {user:?}");  // Phase 4F: Debug trait
            
            // Phase 4D: Async operation
            let result = block_on(save_user(&user));
            match result {
                Ok(_) => console.log("User saved successfully"),
                Err(e) => console.error("Failed to save: {e}")
            }
        },
        Err(e) => console.error("Validation failed: {e}")
    }
}
```

This example demonstrates:
- **Type Safety**: Newtype pattern prevents mixing user IDs with other numbers
- **Memory Safety**: Borrowing ensures efficient access without copying
- **Compile-Time Checks**: Contracts verify preconditions
- **Effect Tracking**: Function signatures show what side effects occur
- **Async Operations**: Non-blocking database access
- **Pattern Matching**: Clean error handling
- **Modern Syntax**: String interpolation, optional chaining, null coalescing
- **Metaprogramming**: Automatic trait implementations

---

### Learning More About Phase 4

Phase 4 includes 62 features - far more than we can cover in this tutorial. For detailed documentation:

📖 **[Phase 4 Features Documentation](PHASE4_FEATURES.md)** - Complete guide to all 62 features with examples

**Quick overview of what's available:**

- **Memory Safety**: Lifetimes, borrowing, move semantics, RAII
- **Type System**: Union types, GADTs, higher-kinded types, refinement types, dependent types
- **Functional**: Partial application, currying, lenses, transducers, do-notation
- **Concurrency**: Async/await, channels, futures, atomic operations, parallel iterators
- **Control Flow**: Try blocks, guard clauses, labelled blocks, defer, panic handling
- **Metaprogramming**: Declarative macros, procedural macros, reflection, code generation
- **Domain Features**: Regex literals, format strings, operator overloading, destructuring
- **Utilities**: Ranges with step, zip, enumerate, default parameters, const functions

---

## Building Real Applications

Now that you've learnt all the features, let's build something real!

### Example: A Simple Web API Client

```rustscript
// Using Phase 1-4 features together
struct ApiClient {
    base_url: string,
    token: Option<string>
}

impl ApiClient {
    fn new(url: string) -> ApiClient {
        ApiClient { base_url: url, token: None }
    }
    
    fn with_token(&mut self, token: string) {
        self.token = Some(token);
    }
    
    async fn get(&self, endpoint: string) -> Result<string, Error>
        effect [io, throws]
        requires !endpoint.is_empty(), "Endpoint cannot be empty"
    {
        let url = "{self.base_url}/{endpoint}";
        let headers = self.token?.map(|t| "Authorization: Bearer {t}") ?? [];
        
        let response = await http.get(url, headers)?;
        Ok(response.text())
    }
}

fn main() {
    let mut client = ApiClient::new("https://api.example.com");
    client.with_token("secret_token");
    
    let result = block_on(async {
        let data = await client.get("users")?;
        console.log("Users: {data}");
        Ok(())
    });
    
    match result {
        Ok(_) => console.log("Success!"),
        Err(e) => console.error("Error: {e}")
    }
}
```

This example uses:
- **Phase 1**: String interpolation, optional chaining, null coalescing
- **Phase 2**: Pattern matching (in match expression)
- **Phase 3**: Design by contract (requires), effect system
- **Phase 4**: Async/await, Result type, lifetimes

---

## Next Steps

Congratulations! You've completed the RustScript tutorial. You now understand:

✅ Basic syntax and control flow  
✅ Data structures and pattern matching  
✅ Error handling with Result and Option  
✅ Phase 1: Modern syntax enhancements  
✅ Phase 2: Advanced function capabilities  
✅ Phase 3: Safety and metaprogramming  
✅ Phase 4: Advanced language features (62 features)

### Continue Learning

- 📖 [Phase 4 Features](PHASE4_FEATURES.md) - Detailed documentation of all 62 advanced features
- ⚡ [Quick Reference](QUICK_REFERENCE.md) - Syntax cheat sheet
- 💻 [Examples Directory](../examples/) - Working code examples
- 🤝 [Contributing Guide](../CONTRIBUTING.md) - Help improve RustScript

### Join the Community

- Report bugs and request features on GitHub
- Share your RustScript projects
- Help improve documentation

Happy coding! 🦀✨

