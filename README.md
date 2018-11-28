# A New Stan-to-C++ Compiler
This repo contains work in progress on a new compiler for Stan, written in OCaml.

## To Get Started

### To build, test, and run
Check out `docker/setup_dev_env.sh` to see how we recommend installing our pre-reqs.

To build `stanc.exe`, run `make`. The binary will be built in `_build/default`

To run tests, run `dune runtest` and use `dune promote` to accept changes.
To run e.g. only the integration tests, run `dune runtest test/integration`.

To auto-format the OCaml code (sadly, this does not work for the two ocamllex
and menhir files), run ` dune build @fmt ` or  `make format`.
To accept the changes proposed by ocamlformat, run `dune promote`.

Run `./_build/default/stanc.exe` on individual .stan file to compile it. Use `-?` to get command line options for debugging.

Use `dune build @update_messages` to see if your additions to the parser have added any new error message possibilities, and `dune promote` to accept them.

## Project Timeline
### Code has been written for the following components:
- A lexer
- A LR(1) parser (without any shift/reduce conflicts), constructing an AST
- A typed and untyped AST
- Command line debugging flags for writing out parsing operations and resulting (decorated or undecorated) AST as s-expression in case of a successful parse / semantic check
- Ported all function signatures from Stan Math
- A well-tested semantic/type checker with informative semantic error messages
- Lexical position printed in syntactic and semantic error messages
- Tests for all models in `stan/src/test/test-models/good` (including the pretty printing functionality) and `stan/src/test/test-models/bad`
- 100% coverage of parse errors with informative custom syntax errors implemented using Menhir's Incremental API
- Added hundreds of extra bad Stan models to test errors (all the models in `stan/src/example-bad/new`) to obtain 100% coverage of all possible parse errors
- A pretty printer for Stan models
- Work in progress on intermediate representations and code generation

### TODO for initial release
- Decide on final tree representation used for AST and IRs, some inspirational ideas:
    - [polymorphic variants](https://github.com/links-lang/links/blob/master/core/types.ml)
    - currently using [Neel's "two-level types" pattern](http://lambda-the-ultimate.org/node/4170#comment-63836)
- End-to-end model test framework
    - Could show generated C++ code matches stanc2 or that the same results are achieved at runtime
- Unit or expect tests at a decent granularity
- Code review
- Write code generation phase
- Continuous integration and deployment for Windows, Linux, and Mac static binaries
- Macro pre-processor with correct mapping of error locations

### The bright road ahead
- Traditional compiler optimisations, like loop optimisations, constant-folding, inlining, CSE, DCE, LICM, auto vectorisation/parallelisation, algebraic simplification, ...) from AST
- Add new features to the language (like type inference, closures, higher order functions, new datatypes, new variable transforms, enumeration of discrete parameters...)


## Important simultaneous work also needed for other reasons
1. `install_tensorflow()` style installers for R and Python that install a C++ toolchain in the user's home directory. We will need this to install the new `stanc` binary.
1. Work needed to compile the math library ahead of time!

## Architectural goals for the new compiler
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* **Optimizing** - takes advantage of info known at the Stan language level.

### Distinct Stanc Phases
1. Parse Stan language into AST that represents the syntax quite closely and aides in development of pretty-printers and linters
1. Typecheck & add type information
1. De-sugar into [Middle Intermediate Representation](https://blog.rust-lang.org/2016/04/19/MIR.html)
1. Analyze & optimize MIR -> MIR (will be many passes)
1. Interpret MIR, emit C++ (or LLVM IR, or Tensorflow)

### Potential Optimizations
* Data and parameters are never modified
* Conditionally independent code-motion
* `target+=` is commutative
* Pattern rewrites; `exp(x) - 1` -> `exp1m(x)`

### AST and IR design considerations
* The AST should have different variant types for each different type of syntax, and thus follow closely. Think about how a pretty-printer would want to deal with an AST (thanks @jimtla!)
* The AST should keep track of debug information (line number, etc) in each node itself, rather than in some external data structures keyed off nodes.
This is so that when we run an optimization pass, we will be forced to design how our AST operations affect line numbers as well as the semantics, and at the end of the day we can always point a user to a specific place in their Stan code.
* We should also keep track of the string representation of numeric literals so we can make sure not to convert accidentally and lose precision.
* It would be nice to have different types for side-effect free code. We might need to analyze for print statements, or possibly ignore them as they are moved around.
* We would prefer to keep track of flow dependencies via MIR CFG pointers to other MIR nodes or symbols rather than via SSA or other renaming schemes.

## Historical context

### Pain points with the current `stanc` architecture
1. C++ is a pain to write optimization and type-checking passes in; adding a language feature touches 40+ files
2. No one has wanted to work on the compiler (probably because of C++ + Spirit Qi)
3. Distribution is a pain (targets C++ and requires C++ toolchain at runtime)
4. Compilation takes a long time.
5. Difficult for possible contributors to jump in - people tend to compile TO Stan, [rewrite a Stan parser in another language](https://github.com/deepppl/yaps/blob/master/yaps/stan.g4), or trick the compiler into emitting the AST as text so they can read it in somewhere else.
6. R and Python interfaces are buggy, hard to install, and time-consuming to maintain

### Ways we could address the pain points
1 and 2) Switch implementation languages to something more expressive and fun
3 and 4) Try to switch to a single binary distribution that ends up either interpreting or linking against something that emits native code.
5) Split up the compiler into many phases with human-readable intermediate representations between the phases
6) Focus on CmdStan as the correct unit of Stan / reference implementation, and jazz it up with some logging I/O.


## Stan 3 language goals
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
