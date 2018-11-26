# A New Stan-to-C++ Compiler
This repo contains work in progress on a new compiler for Stan, written in OCaml.

## To Get Started
### Prerequisites
The project has the following prerequisites:
- GNU Make, which you probably have already
- OCaml programming language, version 4 or later,
- Core(kernel) library
- menhir parser generator
- Dune OCaml build system (formerly known as JBuilder)

### To build, test, and run
To build, run
`
make
`

To test the compiler on all good models in stan/src/test/test-models/good, run (in folder `test`)
`
./run-stan-examples-good.sh
`
This will produce an output file `stan-examples-good-out.log` which will show any errors.

To test the compiler on all bad models in stan/src/test/test-models/bad, run (in folder `test`)
`
./run-stan-examples-bad.sh
`
This will produce an output file `stan-examples-bad-out.log` which will show any errors.

To test the pretty printer on all good models in stan/src/test/test-models/good, run (in folder `test`)
`
./test-pretty-printer.sh
`
This will produce an output file `stan-examples-good-pretty-printed.log` with pretty printed versions of all programs.

To auto-format the OCaml-code (sadly, this does not work for the two ocamllex and menhir files), run 
`
./ocamlformat-stan.sh
`

Run `./_build/default/stan.exe` on individual .stan file to compile it. Use `-?` to get command line options for debugging.

## Done, so far
- A lexer
- A LR(1) parser (without any shift/reduce conflicts), constructing an AST
- A typed and untyped AST
- Command line debugging flags for writing out parsing operations and resulting (decorated or undecorated) AST as s-expression in case of a successful parse / semantic check
- Ported all function signatures from Stan Math
- A well-tested semantic/type checker with informative semantic error messages
- Lexical position printed in syntactic and semantic error messages
- Tested on all models in `stan/src/test/test-models/good` and `stan/src/test/test-models/bad`
- 100% coverage of parse errors with informative custom syntax errors implemented using Menhir's Incremental API
- Added hundreds of extra bad Stan models to test errors (all the models in `stan/src/example-bad/new`) to obtain 100% coverage of all possible parse errors
- A pretty printer for Stan models
- Work in progress on intermediate representations and code generation

## TODO for beta release
- Write code generation phase
- Macro pre-processor with correct mapping of error locations

## Cool stuff to do after
- Create IRs and transforms (embodying compiler optimisations, like loop optimisations, constant-folding, inlining, CSE, DCE, LICM, auto vectorisation/parallelisation, algebraic simplification, ...) from AST
- Add new features to the language (like type inference, closures, higher order functions, new datatypes, new variable transforms, enumeration of discrete parameters...)

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
