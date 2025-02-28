#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y core v0.17.1 --no-action

opam install -y dune core.v0.17.1 menhir.20240715 ppx_deriving.6.1.1 fmt.0.11.0 yojson.2.2.2 cmdliner.2.1.0

eval $(opam env)
