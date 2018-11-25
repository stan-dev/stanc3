# stanc3
This repo will hold a rewrite of the stanc compiler in OCaml.

This repo was created by merging 
https://github.com/seantalts/stan3.git
and
https://github.com/VMatthijs/Stanc-OCaml.git .

The former holds code for an end-to-end prototype of an interpreter for a simple language, including optimization passes,
as well as code generation for the Stan language. The latter holds a parser, AST, semantic check and pretty printer for the Stan language.

Soon, the code from both repos will be integrated and expanded into a full compiler for Stan.
