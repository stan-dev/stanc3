#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y dune 2.8.4 --no-action
opam pin -y core_kernel v0.14.2 --no-action

opam install -y dune.2.8.4 core_kernel.v0.14.2 menhir.20210929 ppx_deriving.5.2.1 fmt.0.8.8 yojson.1.7.0

eval $(opam env)
