#!/bin/bash

# exit when any command fails
set -e

# Cross compiling for windows on debian
eval $(opam env)

# Add windows repository
opam repository add windows http://github.com/ocaml-cross/opam-cross-windows.git

# Install the compiler
opam install -y "ocaml-windows64=4.14.1"

# Install dependencies
opam install -y core.v0.16.1 core-windows.v0.16.1 menhir.20230608 menhir-windows.20230608 ppx_deriving.5.2.1 ppx_deriving-windows.5.2.1\
 fmt.0.9.0 fmt-windows.0.9.0 yojson.2.1.0 yojson-windows.2.1.0 cmdliner.1.3.0 cmdliner-windows.1.3.0 uucp.16.0.0 uucp-windows.16.0.0\
 uunf.16.0.0 uunf-windows.16.0.0

eval $(opam env)
