#!/bin/bash
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam pin -y ocamlformat 0.8
opam pin -y dune 1.11.3

# Add windows repository
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows

#Request the compiler to be built with flambda optimizers
opam install conf-flambda-windows

# Install the compiler
opam install "ocaml-windows64<=4.07.0"

# Install dependencies
opam install -y core_kernel.v0.11.1 menhir.20181113 ppx_deriving.4.2.1 fmt.0.8.5 yojson.1.7.0
opam install -y core_kernel-windows menhir-windows ppx_deriving-windows fmt-windows yojson-windows
