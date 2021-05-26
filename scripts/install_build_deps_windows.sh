#!/bin/bash

# Cross compiling for windows on debian

# Pin dune to 1.11.3 as newer and 2+ do not work for ocaml-windows64 !
opam pin -y dune 1.11.3
opam pin -y opam-file-format 2.1.0

# Add windows repository
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows

#Request the compiler to be built with flambda optimizers
opam install -y conf-flambda-windows

# Install the compiler
opam install -y "ocaml-windows64<=4.07.0"

# Install dependencies
opam install -y core_kernel core_kernel-windows menhir menhir-windows ppx_deriving ppx_deriving-windows fmt fmt-windows yojson yojson-windows

eval $(opam env)
