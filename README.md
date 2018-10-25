This repo is here to explore designs and architectures for new stanc, `stanc3`.
We'll go through some goals of the compiler redesign and new AST,
a roadmap for how the work splits up over time, and goals for additions to the Stan language (these may be moved later).

# Timeline for a New Stanc
1. Create skeleton end-to-end functional interpreters in both Rust an OCaml displaying a minimum non-trivial operation in each module.
    1. We got far enough and chose OCaml for a few reasons - borrow checker, Menhir, expect tests.
1. announce project seeking help,
1. Agree on AST definition (2 weeks)
1. Agree on desugared IR (1 week)
1. Agree on something like [Middle Intermediate Representation](https://blog.rust-lang.org/2016/04/19/MIR.html) equivalent (2 weeks)
1. extend parsing to full Stan 2 language (Matthijs said he did this in day!)
1. type-checking (1 month)
1. backend to emit C++ (1 month)

## Important simultaneous work also needed for other reasons
1. `install_tensorflow()` style installers for R and Python that install a C++ toolchain in the user's home directory. We will need this to install the new `stanc` binary.
1. Refactoring the model class to have a base class, and the algorithms to not be templated (speeds up compile times. @mitzimorris is working on this).
1. Any work to compile the math library ahead of time!


# Architectural goals for the new compiler
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* **Optimizing** - takes advantage of info known at the Stan language level.

## Distinct Stanc Phases
1. Parse Stan language into AST that represents the syntax quite closely and aides in development of pretty-printers and linters
1. Typecheck & add type information
1. De-sugar into [Middle Intermediate Representation](https://blog.rust-lang.org/2016/04/19/MIR.html)
1. Analyze & optimize MIR -> MIR (will be many passes)
1. Interpret MIR, emit C++ (or LLVM IR, or Tensorflow)

## Potential Optimizations
* Data and parameters are never modified
* Conditionally independent code-motion
* `target+=` is commutative
* Pattern rewrites; `exp(x) - 1` -> `exp1m(x)`

## AST and IR design considerations
* The AST should have different variant types for each different type of syntax, and thus follow closely. Think about how a pretty-printer would want to deal with an AST (thanks @jimtla!)
* The AST should keep track of debug information (line number, etc) in each node itself, rather than in some external data structures keyed off nodes.
This is so that when we run an optimization pass, we will be forced to design how our AST operations affect line numbers as well as the semantics, and at the end of the day we can always point a user to a specific place in their Stan code.
* We should also keep track of the string representation of numeric literals so we can make sure not to convert accidentally and lose precision.
* It would be nice to have different types for side-effect free code. We might need to analyze for print statements, or possibly ignore them as they are moved around.
* We would prefer to keep track of flow dependencies via MIR CFG pointers to other MIR nodes or symbols rather than via SSA or other renaming schemes.

# Historical context

## Pain points with the current `stanc` architecture
1. C++ is a pain to write optimization and type-checking passes in; adding a language feature touches 40+ files
2. No one has wanted to work on the compiler (probably because of C++ + Spirit Qi)
3. Distribution is a pain (targets C++ and requires C++ toolchain at runtime)
4. Compilation takes a long time.
5. Difficult for possible contributors to jump in - people tend to compile TO Stan, [rewrite a Stan parser in another language](https://github.com/deepppl/yaps/blob/master/yaps/stan.g4), or trick the compiler into emitting the AST as text so they can read it in somewhere else.
6. R and Python interfaces are buggy, hard to install, and time-consuming to maintain

## Ways we could address the pain points
1 and 2) Switch implementation languages to something more expressive and fun
3 and 4) Try to switch to a single binary distribution that ends up either interpreting or linking against something that emits native code.
5) Split up the compiler into many phases with human-readable intermediate representations between the phases
6) Focus on CmdStan as the correct unit of Stan / reference implementation, and jazz it up with some logging I/O.


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
