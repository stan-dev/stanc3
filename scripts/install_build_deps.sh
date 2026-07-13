#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y base v0.17.3 --no-action

opam install -y dune base.v0.17.3 menhir.20260209 ppx_deriving.6.1.1 fmt.0.11.0 yojson.3.0.0 cmdliner.2.1.1\
     ppx_hash ppx_compare ppx_sexp_conv ppx_expect ppx_inline_test ppx_pipebang ppx_sexp_value ppx_sexp_message

eval $(opam env)
