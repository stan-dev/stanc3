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
logging is turned on, which should let you reproduce the parse)
as well as an error file (which should be empty, unless some files could not
be parsed).



# Note on jbuilder WIP
I need to tinker a bit with jbuilder to get the AST serialisation to build
properly as it relies on an s-expression generator that is part of the
Core library.
