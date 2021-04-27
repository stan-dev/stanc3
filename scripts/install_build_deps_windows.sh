#!/bin/bash

# Cross compiling for windows on debian

# Pin dune to 1.11.3 as newer and 2+ do not work for ocaml-windows64 !
opam pin -y dune 2.8.2
opam pin -y opam-file-format 2.1.0

# Add windows repository
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows

#Request the compiler to be built with flambda optimizers
opam install conf-flambda-windows

# Install the compiler
opam install "ocaml-windows64<=4.11.1"

# Install dependencies
opam install -y core core-windows menhir menhir-windows ppx_deriving ppx_deriving-windows fmt fmt-windows yojson yojson-windows

eval $(opam env)
