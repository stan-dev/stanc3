#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam install -y dune menhir.20260209 ppx_deriving.6.1.1 fmt.0.11.0 yojson.3.0.0 cmdliner.2.1.1\
     ppx_compare ppx_sexp_conv ppx_expect_nobase

eval $(opam env)
