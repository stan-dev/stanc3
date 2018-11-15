# A Starting Point for a New Stan Compiler
This repo contains some initial work on a new compiler for Stan, written in OCaml.

# To Get Started
The project has the following prerequisites:
- GNU Make, which you probably have already
- OCaml programming language, version 4 or later,
- Core(kernel) library
- menhir parser generator.

To build, run
`
make stan
`

To test the parser on all good models in stan/src/test/test-models/good, run
`
./run-stan-examples-good.sh
`
This will produce an output file (containing logging info, presuming that
logging is turned on in `debug.ml`, which should let you reproduce the parse and/or should
print the AST as an s-expression)
as well as an error file (which should be empty, unless some files could not
be parsed).


# Done, so far
- Prototype lexer
- Prototype parser with semantic actions
- Prototype AST
- Debugging flags for writing out parsing operations and resulting AST as s-expression in case of a successful parse
- Tested on all models in `stan/src/test/test-models/good` and `stan/src/test/test-models/bad`
- Ported all function signatures from Stan Math
- Prototype semantic check
- Lexical position printed in syntactic and semantic error messages

# TODO
- Generate better syntax error messages during parsing (use menhir --list-errors to systematically list all paths to a parse error which should get a custom error message)
- Improve quality of semantic error messages
- Create IRs and transforms (embodying compiler optimisations, like loop optimisations, constant-folding, inlining, CSE, DCE, LICM, auto vectorisation/parallelisation, algebraic simplification, ...) from AST
- Create code generation phase from IRs
- Add new features to the language (like type inference, closures, higher order functions, new datatypes, new variable transforms, enumeration of discrete parameters...)
