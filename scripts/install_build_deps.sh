#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

# until next versions of the _nobase packages are published, this avoids a heavy opam-core dependency
opam pin -y ppx_expect_nobase https://github.com/Kakadu/ppx_expect_nobase.git#3436e63f7c94887da735818933c5db138e1dac1a --no-action

opam install -y dune menhir.20260209 ppx_deriving.6.1.1 fmt.0.11.0 yojson.3.0.0 cmdliner.2.1.1\
     ppx_compare ppx_sexp_conv ppx_expect_nobase

eval $(opam env)
