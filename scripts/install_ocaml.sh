#!/bin/bash

# exit when any command fails
set -e


# Create and switch to 4.12.0
# switch can only be created if it doesn't already exist
(opam switch stanc 2> /dev/null && echo "Opam switch 'stanc' already found, leaving it be") || opam switch create stanc ocaml-base-compiler.4.12.0 -y

opam switch stanc

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)
