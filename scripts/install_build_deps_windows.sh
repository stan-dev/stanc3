#!/bin/bash

# Cross compiling for windows on debian

# Add windows repository
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows
opam update

#Request the compiler to be built with flambda optimizers
opam install -y conf-flambda-windows

# Install the compiler
opam install -y "ocaml-windows64=4.12.0"

# Install dependencies
opam install -y core_kernel.v0.14.2 core_kernel-windows.v0.14.2 menhir.20210929 menhir-windows.20210929 ppx_deriving.5.2.1 ppx_deriving-windows.5.2.1 fmt.0.8.8 fmt-windows.0.8.8 yojson.1.7.0 yojson-windows.1.7.0

eval $(opam env)
