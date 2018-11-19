# A Starting Point for a New Stan Compiler
This repo contains some initial work on a new compiler for Stan, written in OCaml.

## To Get Started
The project has the following prerequisites:
- GNU Make, which you probably have already
- OCaml programming language, version 4 or later,
- Core(kernel) library
- menhir parser generator.

To build, run
`
make stan
`

To test the compiler on all good models in stan/src/test/test-models/good, run
`
./run-stan-examples-good.sh
`

To test the compiler on all bad models in stan/src/test/test-models/bad, run
`
./run-stan-examples-bad.sh
`
This will produce an output file which will show any errors.

To auto-format the OCaml-code (sadly, this does not work for the two ocamllex and menhir files), run 
`
./ocamlformat-stan.sh
`

Run `./stan.native` on individual .stan file to compile it. Use `-?` to get command line options for debugging.

Run `./compile_menhir_errors.sh` to compile custom syntax errors if you have added any in the `src/stan/parser.messages` file.

## Done, so far
- A lexer
- A LR(1) parser (without any shift/reduce conflicts), constructing an AST
- A typed and untyped AST
- Command line debugging flags for writing out parsing operations and resulting (decorated or undecorated) AST as s-expression in case of a successful parse / semantic check
- Ported all function signatures from Stan Math
- Prototype semantic check
- Lexical position printed in syntactic and semantic error messages
- Tested on all models in `stan/src/test/test-models/good` and `stan/src/test/test-models/bad`
- Good custom syntax errors implemented using Menhir's Incremental API
- Added hundreds of extra bad Stan models to test errors (all the models in `stan/src/example-bad/new`)

## TODO
- Perfect quality of syntax error messages by inserting them in the `src/stan/parser.messages` file.
- Improve quality of semantic error messages
- Create IRs and transforms (embodying compiler optimisations, like loop optimisations, constant-folding, inlining, CSE, DCE, LICM, auto vectorisation/parallelisation, algebraic simplification, ...) from AST
- Create code generation phase from IRs
- Macro pre-processor with correct mapping of error locations
- Add new features to the language (like type inference, closures, higher order functions, new datatypes, new variable transforms, enumeration of discrete parameters...)
