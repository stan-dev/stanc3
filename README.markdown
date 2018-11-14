# The Programming Languages Zoo

This project builds on the PL Zoo.

Copyright © 2016 Andrej Bauer, Matija Pretnar

The Programming Languages Zoo, or *PL Zoo* for short, is a collection of implementations
of miniature programming languages which demonstrates various techniques used in
implementation of programming languages. It is a good starting point for those who would
like to implement their own programming language, or just learn how it is done.

See the [PL Zoo website](http://plzoo.andrej.com/) for further information, including
installation instructions.

# A Starting Point for a New Stan Compiler
I have chosen to use Andrej Bauer Matija Pretnar PL Zoo as a starting point for implementing a compiler for Stan,
as I found it instructive for ultra simple examples to get started. Eventually, of course,
we'd get rid of the other languages and integrate the shared components.

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
- tested on all models in `stan/src/test/test-models/good`
- Ported all function signatures from Stan Math
- Prototype semantic check
- Lexical position printed in syntactic and semantic error messages

# TODO
- Generate better syntax error messages during parsing (use menhir --list-errors to systematically list all paths to a parse error which should get a custom error message)
- Improve quality of semantic error messages
- Create IRs and transforms (embodying compiler optimisations, like loop optimisations, constant-folding, inlining, CSE, DCE, LICM, auto vectorisation/parallelisation, algebraic simplification, ...) from AST
- Create code generation phase from IRs
- Add new features to the language (like type inference, closures, higher order functions, new datatypes, new variable transforms, enumeration of discrete parameters...)
