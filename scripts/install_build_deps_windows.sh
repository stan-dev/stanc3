#!/bin/bash

# Pin dune to 1.11.3 as newer and 2+ do not work for ocaml-windows64 !
opam pin -y dune 1.11.3

# Add windows repository
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows

#Request the compiler to be built with flambda optimizers
opam install conf-flambda-windows

# Install the compiler
opam install "ocaml-windows64<=4.07.0"

# Install dependencies
opam install -y core_kernel-windows menhir-windows ppx_deriving-windows fmt-windows yojson-windows

eval $(opam env)