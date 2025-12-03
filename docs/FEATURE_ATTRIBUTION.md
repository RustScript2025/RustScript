# RustScript Feature Attribution

**Author**: Michael Lauzon

This document provides complete attribution for all 87 features in RustScript, showing which languages inspired each feature and when those languages were created.

RustScript draws from **60+ languages** spanning **68 years** of computing history (1958-2025).

---

## Phase 1: String & Syntax Enhancements (4 features)

### String Interpolation
**Syntax**: `"Hello, {name}!"`  
**Inspired by**: Ruby (1995), Python (2015), Kotlin (2011), JavaScript ES6 (2015), Swift (2014)  
**Why**: Eliminates error-prone string concatenation and makes code more readable

### Optional Chaining
**Syntax**: `user?.address?.city`  
**Inspired by**: Swift (2014), TypeScript (2020), C# (2015), Kotlin (2011)  
**Why**: Safe navigation through potentially null values without verbose null checks

### Null Coalescing
**Syntax**: `value ?? "default"`  
**Inspired by**: C# (2000), Swift (2014), PHP (2009), JavaScript (2020)  
**Why**: Concise default value handling without if-else chains

### List Comprehensions
**Syntax**: `[x * 2 for x in numbers if x > 0]`  
**Inspired by**: Python (1994), Haskell (1990), Scala (2004), F# (2005)  
**Why**: Declarative collection transformation with mathematical clarity

---

## Phase 2: Function Enhancements (3 features)

### Pattern Matching in Function Heads
**Syntax**: Multiple function definitions with different patterns  
**Inspired by**: Erlang (1986), Elixir (2011), Haskell (1990), ML (1973)  
**Why**: Elegant handling of different input cases without nested if-else

### Generators
**Syntax**: `gen fn name() { yield value; }`  
**Inspired by**: Python (2001), JavaScript ES6 (2015), C# (2005), Icon (1977)  
**Why**: Memory-efficient lazy evaluation for infinite sequences

### Multiple Dispatch
**Syntax**: Type-based function selection  
**Inspired by**: Julia (2012), Common Lisp CLOS (1988), Dylan (1992), Clojure (2009)  
**Why**: Symmetric treatment of all arguments in function dispatch

---

## Phase 3: Safety & Contracts (3 features)

### Design by Contract
**Syntax**: `requires`/`ensures`/`invariant`  
**Inspired by**: Eiffel (1986), D (2001), Ada (1983), Spec# (2004)  
**Why**: Formal specification of function behaviour catches bugs that tests miss

### Effect System
**Syntax**: `effect [pure, io, state, ...]`  
**Inspired by**: Koka (2012), Eff (2012), Nim (2008), Rust traits  
**Why**: Track and control side effects for easier reasoning about code

### Compile-time Execution
**Syntax**: `comptime { ... }`  
**Inspired by**: Zig (2016), D CTFE (2007), C++ constexpr (2011), Nim (2008)  
**Why**: Move computation from runtime to compile time for performance and safety

---

## Phase 4: Advanced Language Features (77 features)

### Phase 4A: Core Memory Safety (10 features)

**Lifetimes**  
*Inspired by*: Rust (2010), Cyclone (2002)  
*Why*: Prevent dangling references at compile time

**Borrowing & References**  
*Inspired by*: Rust (2010)  
*Why*: Multiple readers OR one writer prevents data races

**Move Semantics**  
*Inspired by*: Rust (2010), C++ (2011)  
*Why*: Explicit ownership transfer prevents use-after-free

**Tail Call Optimisation**  
*Inspired by*: Scheme (1975), Lua (1993)  
*Why*: Infinite recursion without stack overflow

**Pattern Guards**  
*Inspired by*: Haskell (1990), Erlang (1986)  
*Why*: Add conditions to pattern matching

**Traits (RAII & Drop)**  
*Inspired by*: Rust (2010), C++ RAII (1984)  
*Why*: Automatic resource management

**Const Generics**  
*Inspired by*: Rust (2020), C++ templates (1990)  
*Why*: Type-level constants for array sizes

**Algebraic Effects**  
*Inspired by*: Eff (2012), Koka (2012)  
*Why*: Composable side effect handling

**Inline Assembly**  
*Inspired by*: Rust (2015), C (1972)  
*Why*: Low-level hardware access when needed

**Complete Trait System**  
*Inspired by*: Rust (2010), Haskell type classes (1988)  
*Why*: Polymorphism without inheritance

### Phase 4B: Advanced Type System (15 features)

**Union & Intersection Types**  
*Inspired by*: TypeScript (2012), Ceylon (2011)  
*Why*: Flexible type combinations

**Type Aliases**  
*Inspired by*: Haskell (1990), Rust (2010), TypeScript (2012)  
*Why*: Readable names for complex types

**Newtype Pattern**  
*Inspired by*: Haskell (1990), Rust (2010)  
*Why*: Zero-cost type safety

**Associated Types**  
*Inspired by*: Rust (2015), Haskell (1996)  
*Why*: Type families in traits

**Higher-Kinded Types**  
*Inspired by*: Haskell (1990), Scala (2004)  
*Why*: Abstract over type constructors

**Phantom Types**  
*Inspired by*: Haskell (1990), Rust (2010)  
*Why*: Compile-time type safety with zero runtime cost

**Refinement Types**  
*Inspired by*: Liquid Haskell (2008), F* (2011)  
*Why*: Types with predicates for stronger guarantees

**Dependent Types**  
*Inspired by*: Idris (2007), Agda (2007), Coq (1989)  
*Why*: Types that depend on values

**Type-Level Programming**  
*Inspired by*: Haskell (1990), TypeScript (2012)  
*Why*: Computation at the type level

**Existential Types**  
*Inspired by*: Haskell (1990), Rust impl Trait (2018)  
*Why*: Hide concrete types behind interfaces

**GADTs (Generalised Algebraic Data Types)**  
*Inspired by*: Haskell (2003), OCaml (2004)  
*Why*: Type-safe expression trees

**Variance Annotations**  
*Inspired by*: Scala (2004), Kotlin (2011)  
*Why*: Control subtyping relationships

**Type Bounds**  
*Inspired by*: Rust (2010), Haskell (1988), Java (2004)  
*Why*: Constrain generic types

**Subtyping**  
*Inspired by*: Scala (2004), TypeScript (2012)  
*Why*: Type hierarchies without inheritance

**Structural Typing**  
*Inspired by*: TypeScript (2012), Go (2009)  
*Why*: Duck typing with compile-time checks

### Phase 4C: Functional Programming (10 features)

**Partial Application**  
*Inspired by*: Haskell (1990), ML (1973), F# (2005)  
*Why*: Fix some arguments to create specialized functions

**Function Composition**  
*Inspired by*: Haskell (1990), F# (2005), Elixir (2011)  
*Why*: Chain operations left-to-right or right-to-left

**Currying**  
*Inspired by*: Haskell (1990), ML (1973), OCaml (1996)  
*Why*: Transform multi-parameter functions into chains

**Lazy Evaluation**  
*Inspired by*: Haskell (1990), Miranda (1985)  
*Why*: Defer computation until needed

**Memoisation**  
*Inspired by*: Common Lisp (1984), Python decorators (2004)  
*Why*: Cache function results automatically

**Immutable Data Structures**  
*Inspired by*: Clojure (2007), Scala (2004)  
*Why*: Persistent collections with structural sharing

**Transducers**  
*Inspired by*: Clojure (2014)  
*Why*: Composable algorithmic transformations

**Do-Notation**  
*Inspired by*: Haskell (1990)  
*Why*: Monadic composition with imperative syntax

**Applicative Functors**  
*Inspired by*: Haskell (2008)  
*Why*: Apply functions in computational contexts

**Lenses**  
*Inspired by*: Haskell (2012)  
*Why*: Composable getters and setters for immutable data

### Phase 4D: Concurrency & Parallelism (8 features)

**Async/Await**  
*Inspired by*: C# (2012), JavaScript (2017), Rust (2019)  
*Why*: Write asynchronous code that looks synchronous

**Channels**  
*Inspired by*: Go (2009), Rust (2015)  
*Why*: Message passing between concurrent tasks

**Futures & Streams**  
*Inspired by*: Scala (2010), Rust (2016)  
*Why*: Composable asynchronous operations

**Mutex & RwLock**  
*Inspired by*: Rust (2010), C++ (2011)  
*Why*: Safe shared-state concurrency

**Atomic Operations**  
*Inspired by*: C++ (2011), Rust (2015)  
*Why*: Lock-free synchronisation primitives

**Parallel Iterators**  
*Inspired by*: Rayon/Rust (2016)  
*Why*: Data parallelism with iterator interface

**Scoped Threads**  
*Inspired by*: Rust (2022)  
*Why*: Safe borrowing across thread boundaries

**Select (Channel Multiplexing)**  
*Inspired by*: Go (2009), Rust (2015)  
*Why*: Wait on multiple channels simultaneously

### Phase 4E: Advanced Control Flow (10 features)

**Try Blocks**  
*Inspired by*: Rust (2018), Kotlin (2011)  
*Why*: Handle multiple error types elegantly

**Try Operator (?)**  
*Inspired by*: Rust (2016), Swift (2014)  
*Why*: Propagate errors concisely

**Guard Clauses**  
*Inspired by*: Swift (2014), Ruby (1995)  
*Why*: Early returns reduce nesting

**Labelled Blocks**  
*Inspired by*: Rust (2015), Java (1995)  
*Why*: Break from nested loops with values

**Catch Expressions**  
*Inspired by*: Kotlin (2011), Scala (2004)  
*Why*: Inline error handling

**Panic with Backtraces**  
*Inspired by*: Rust (2015), Go (2009)  
*Why*: Unrecoverable errors with debugging info

**Defer Statements**  
*Inspired by*: Go (2009), Swift (2014), Zig (2016)  
*Why*: Guaranteed cleanup code execution

**Conditional Compilation**  
*Inspired by*: Rust (2015), C (1972)  
*Why*: Platform-specific code

**Const Assertions**  
*Inspired by*: Rust (2019), C++ (2011)  
*Why*: Compile-time validation

**Unreachable Markers**  
*Inspired by*: Rust (2015), Swift (2014)  
*Why*: Document impossible code paths

### Phase 4F: Metaprogramming & Macros (8 features)

**Declarative Macros**  
*Inspired by*: Rust macro_rules! (2015), Lisp (1958)  
*Why*: Pattern-based code generation

**Procedural Macros**  
*Inspired by*: Rust (2018), Lisp (1958)  
*Why*: Arbitrary code transformation

**Attribute Macros**  
*Inspired by*: Rust (2018), Java annotations (2004)  
*Why*: Annotate and transform declarations

**Compile-Time Reflection**  
*Inspired by*: Zig (2016), D (2001)  
*Why*: Type introspection at compile time

**Code Generation**  
*Inspired by*: Zig comptime (2016), D CTFE (2007)  
*Why*: Generate code during compilation

**Quasiquoting**  
*Inspired by*: Lisp (1960s), Rust quote! (2016)  
*Why*: Template-based code generation

**Hygiene**  
*Inspired by*: Scheme (1986), Rust (2015)  
*Why*: Prevent variable capture in macros

**Syntax Extensions**  
*Inspired by*: Rust (2015), Scala (2004), Nim (2008)  
*Why*: Extend language syntax

### Phase 4G: Domain-Specific Features (6 features)

**Regex Literals**  
*Inspired by*: Perl (1987), JavaScript (1995), Ruby (1995)  
*Why*: First-class pattern matching

**Format Strings**  
*Inspired by*: Python f-strings (2015), Rust (2018)  
*Why*: Type-safe string formatting

**String Slicing**  
*Inspired by*: Python (1991), Rust (2015)  
*Why*: Efficient substring operations

**Operator Overloading**  
*Inspired by*: C++ (1983), Rust (2015), Python (1991)  
*Why*: Custom operators for user types

**Custom Indexing**  
*Inspired by*: C++ (1983), Rust (2015), Python (1991)  
*Why*: Implement `[]` operator for types

**Destructuring Assignment**  
*Inspired by*: JavaScript ES6 (2015), Rust (2015), Python (1991)  
*Why*: Extract values from complex structures

### Phase 4H: Additional Utilities (5 features)

**Ranges with Step**  
*Inspired by*: Python (1991), Ruby (1995), Rust (2015)  
*Why*: Custom iteration increments

**Zip Iterator**  
*Inspired by*: Python (1991), Haskell (1990), Rust (2015)  
*Why*: Combine multiple iterators

**Enumerate**  
*Inspired by*: Python (1991), Rust (2015)  
*Why*: Add indices to iteration

**Default Parameters**  
*Inspired by*: Python (1991), JavaScript (2015), Rust (2021)  
*Why*: Optional function arguments

**Const Functions**  
*Inspired by*: Rust (2018), C++ constexpr (2011)  
*Why*: Functions callable at compile time

### Phase 4I: MUSHcode-Inspired Features (5 features)

**Iteration Placeholders**  
*Inspired by*: MUSHcode iter() (1990)  
*Why*: Concise iteration with `##` for value and `#@` for index

**Register Variables**  
*Inspired by*: MUSHcode setq()/setr() (1990)  
*Why*: Fast temporary storage with `%q0`-`%q9`

**String Registers**  
*Inspired by*: MUSHcode string accumulation (1990)  
*Why*: Efficient string building with `%r0`-`%r9`

**Literal Operator**  
*Inspired by*: MUSHcode lit() (1990)  
*Why*: Code-as-data patterns with `lit!()`

**Default Function**  
*Inspired by*: MUSHcode default()/edefault() (1990)  
*Why*: Flexible fallback handling beyond null coalescing

---

## Language Timeline

### 1950s-1960s
- **LISP (1958)**: Macros, first-class functions, homoiconicity
- **SNOBOL (1962)**: Pattern matching
- **APL (1966)**: Array operations
- **MUMPS (1966)**: Persistent data

### 1970s
- **Forth (1970)**: Stack-based operations
- **C (1972)**: Inline assembly, conditional compilation
- **ML (1973)**: Type inference, pattern matching
- **Scheme (1975)**: Tail call optimisation, lexical scoping
- **AWK (1977)**: Pattern-action paradigm
- **Icon (1977)**: Generators

### 1980s
- **Ada (1983)**: Design by Contract
- **C++ (1983)**: RAII, operator overloading
- **Common Lisp (1984)**: Memoisation
- **Erlang (1986)**: Pattern matching in function heads
- **Eiffel (1986)**: Design by Contract (requires/ensures)
- **CLOS (1988)**: Multiple dispatch
- **Perl (1987)**: Regex literals

### 1990s
- **Haskell (1990)**: Pure functional programming, lazy evaluation, type classes, lenses
- **MUSHcode (1990)**: Iteration placeholders, registers, literal operator, default function
- **Python (1991)**: List comprehensions, generators, default parameters
- **Dylan (1992)**: Multiple dispatch
- **Lua (1993)**: Tail call optimisation
- **Ruby (1995)**: String interpolation, guard clauses
- **JavaScript (1995)**: Async programming, regex literals
- **OCaml (1996)**: GADTs, functional programming
- **Java (1995)**: Labelled blocks

### 2000s
- **C# (2000)**: Null coalescing, async/await
- **D (2001)**: Compile-time execution, contracts
- **Cyclone (2002)**: Lifetimes
- **Scala (2004)**: Variance, subtyping, catch expressions
- **F# (2005)**: Partial application, function composition
- **C# (2005)**: Generators
- **Idris (2007)**: Dependent types
- **Agda (2007)**: Dependent types
- **Liquid Haskell (2008)**: Refinement types
- **Nim (2008)**: Effect system, syntax extensions
- **Clojure (2007)**: Immutable data structures
- **Go (2009)**: Channels, defer, panic, select
- **TypeScript (2012)**: Union types, structural typing

### 2010s-2020s
- **Rust (2010-2022)**: Lifetimes, borrowing, move semantics, traits, const generics, async/await, macros, and much more
- **Kotlin (2011)**: Null safety, try blocks
- **Julia (2012)**: Multiple dispatch
- **Koka (2012)**: Effect system, algebraic effects
- **Eff (2012)**: Algebraic effects
- **Swift (2014)**: Optional chaining, guard clauses, try operator
- **Clojure (2014)**: Transducers
- **Python (2015)**: f-strings
- **JavaScript ES6 (2015)**: Template literals, destructuring, generators
- **Zig (2016)**: Compile-time execution (comptime), defer
- **Rayon (2016)**: Parallel iterators

---

## Summary

RustScript's 87 features represent a carefully curated selection from 68 years of programming language evolution. Each feature was chosen because it solved a real problem elegantly in its original language and has been proven in production systems.

By standing on the shoulders of giants, RustScript avoids repeating past mistakes while embracing proven solutions, creating a modern language that feels familiar to developers from many backgrounds.
