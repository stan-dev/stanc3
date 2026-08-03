#!/bin/bash

# exit when any command fails
set -e

# Cross compiling for windows on debian
eval $(opam env)

# Add windows repository
opam repository add windows http://github.com/ocaml-cross/opam-cross-windows.git

# Install the compiler
opam pin add -y ocaml-windows 5.5.0

# Install dependencies
opam install -y menhir.20260209 menhir-windows.20260209 fmt.0.11.0 fmt-windows.0.11.0\
     yojson.3.0.0 yojson-windows.3.0.0 cmdliner.2.1.1 cmdliner-windows.2.1.1 ppx_deriving.6.1.1\
     ppx_deriving-windows.6.1.1 ppx_compare ppx_compare-windows ppx_sexp_conv ppx_sexp_conv-windows\
     ppx_expect_nobase ppx_expect_nobase-windows ppx_sexp_value ppx_sexp_value_windows

eval $(opam env)
