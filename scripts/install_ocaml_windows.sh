#!/bin/bash

# Cross compiling for windows on debian

# The following command installs the 2.0.4 opam version on debian systems.
wget https://github.com/ocaml/opam/releases/download/2.0.4/opam-2.0.4-x86_64-linux
sudo install opam-2.0.4-x86_64-linux /usr/local/bin/opam

# Initialize opam
opam init $1

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)

# Create and switch to 4.12.0
opam switch create stanc ocaml-base-compiler.4.12.0
opam switch stanc

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)



