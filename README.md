This repo is here to explore designs and architectures for new stanc, `stanc3`.
We'll go through some goals of the compiler redesign and new AST,
a roadmap for how the work splits up over time, and goals for additions to the Stan language (these may be moved later).

# Architectural goals for the new compiler and/or interpreter
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* Keep **line number** information throughout all phases through to runtime - there are some errors that only pop up during sampling (key example - discrete parameters do not work with HMC).
* First, an **interpreter** - improves compile times and distribution. Later, JIT with LLVM.
* **Optimizing** - takes advantage of info known at the Stan language level.
* **Retain info at runtime** - e.g. `isFinite(X)` can be checked once, matrix sizes (useful for JIT)

## Distinct Phases
1. Parse Stan language into AST
1. Typecheck AST
1. Analyze & optimize AST -> IR (could be many passes)
1. Interpret IR

## Potential Optimizations
* Data and parameters are never modified
* Conditionally independent code-motion
* `target+=` is commutative
* Pattern rewrites; `exp(x) - 1` -> `exp1m(x)`

# Roadmap
## Part 1 - all high level bullets can be in parallel
1. Get a simple language working end to end in OCaml. This means all 4 phases (though all can be simple). Each of these phases should be able to be worked on in parallel as well.
    1. Parse a simple arithmetic expression language
    1. TODO: Typechecking phase
    1. TODO: Optimization phase
    1. Interpret the AST
    1. TODO: FFI with simple AOT compiled Math library
    1. TODO: Compile and link against HMC algorithm
    1. TODO(optional): Emit C++ code
1. Fix the model concept such that there are non-templated versions available.
1. Compile the math library ahead of time for just 1 container type per argument
1. Write code to generate Ocaml FFI wrappers
1. Write code to generate C++ `extern "C"` wrappers

## Part 2
1. Get Stan 2 parsing and input AST defined.
1. Expand FFI coverage to all of Math library for a single container type
1. Figure out IR - maps? new data types?
1. Pretty print AST sexp
1. More optimizations ;)

# Interpreter design
1. Lex and parse Stan 2+3 into a typed AST that retains type and line number information
1. Typecheck AST: Returns "OK" or some error with line numbers.
1. Optimize AST: Returns a new AST and populates a set of data structures from AST node to arbitrary metadata to capture information from flow analysis, etc.
1. Interpret! This stage must have FFI externs to C from both the OCaml side and the C++ side (to wrap C++ functions in C wrappers we can call from OCaml).

## AST
I think it behooves us to make some simplifications in AST design while adding more metadata.

* For example, I can't think of good reasons to keep track of whether some function application is in fact a binary operation, unary operation, or function call. This may end up being extended to other special forms like if, for, while, etc.
* The AST should probably keep track of debug information (line number, etc) in each node itself, rather than in some external data structures keyed off nodes. This is so that when we run an optimization pass, we will be forced to design how our AST operations affect line numbers as well as the semantics, and at the end of the day we can always point a user to a specific place in their Stan code.
* We should also keep track of the string representation of numeric literals so we can make sure not to convert accidentally and lose precision.
* It would be nice to have different types for side-effect free code. We might need to analyze for print statements, or possibly ignore them as they are moved around.
* We would prefer to keep track of flow dependencies via AST pointers to other AST nodes or symbols rather than via SSA or other renaming schemes.

# Stan 3 language goals
* Make it easier for users to share code (modularity and encapsulation are important here)
* Make it easier for users to compose models together
* Force users to learn as little as possible to get numerical stability and performance (looking at you, `transformed data`)
* Capture arbitrary metadata about AST nodes or variables:
    - @silent do not save these values
    - @prior tag on AST for automatic SBC, PPC
    - @opencl on matrix types to send to GPU
    - @hierarchical_params for GMO et al
    - ??? @broadcast
    - ??? @genquant
    - ??? constraints (`lower=0`, `corr_matrix`) ???
* User-defined derivatives
* tuples or structs
* missing data
* automated vectorization
* `extern` for FFI w/ gradients
