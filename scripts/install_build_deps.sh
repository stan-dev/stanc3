#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y core v0.17.2 --no-action

opam install -y dune core.v0.17.2 menhir.20260209 ppx_deriving.6.1.1 fmt.0.11.0 yojson.3.0.0 cmdliner.2.1.1

eval $(opam env)
