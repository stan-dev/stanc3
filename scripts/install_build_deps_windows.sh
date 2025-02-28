#!/bin/bash

# exit when any command fails
set -e

# Cross compiling for windows on debian
eval $(opam env)

# Add windows repository
opam repository add windows http://github.com/ocaml-cross/opam-cross-windows.git

# Install the compiler
opam install -y ocaml-windows

# Install dependencies
opam install -y core.v0.17.1 core-windows.v0.17.1 menhir.20240715 menhir-windows.20240715 ppx_deriving.6.1.1 ppx_deriving-windows.6.1.1\
 fmt.0.11.0 fmt-windows.0.11.0 yojson.2.2.2 yojson-windows.2.2.2 cmdliner.2.1.0 cmdliner-windows.2.1.0

eval $(opam env)
