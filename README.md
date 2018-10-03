# Architectural goals
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* Keep **line number** information throughout all phases through to runtime - there are some errors that only pop up during sampling (key example - discrete parameters do not work with HMC).
* First, an **interpreter** - improves compile times and distribution. Later, JIT with LLVM.
* **Optimizing** - takes advantage of info known at the Stan language level.
* **Retain info at runtime** - e.g. `isFinite(X)` can be checked once, matrix sizes (useful for JIT)

## Phases
1. Parse Stan language into AST
1. Typecheck AST
1. Analyze & optimize AST -> IR (could be many passes)
1. Interpret IR

## Potential Optimizations
* Data and parameters are never modified
* Conditionally independent code-motion
* `target+=` is commutative
* Pattern rewrites; `exp(x) - 1` -> `exp1m(x)`

# Stan 3 language goals
* Make it easier for users to share code (modularity and encapsulation are important here)
* Make it easier for users to compose models together
* Force users to learn as little as possible to get numerical stability and performance (looking at you, `transformed data`)
* Single, general-purpose annotation system
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

# Roadmap
1. Get a simple language working end to end in Rust (and maybe OCaml to compare). This means all 4 phases (though optimization can be simple).
    1. TODO: Typechecking phase
    1. TODO: Optimization phase
    1. TODO: FFI with simple AOT compiled Math library
    1. TODO: Compile and link against HMC algorithm
1. Get Stan 2 parsing and input AST defined.
1. Expand FFI coverage to all of Math library for a single container type
1. Figure out IR - maps? new data types?
1. Pretty print AST sexp
1. More optimizations ;)
