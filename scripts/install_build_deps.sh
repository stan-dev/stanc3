#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y base v0.17.3 --no-action

opam install -y dune base.v0.17.3 stdio.v0.17.0 menhir.20260209 ppx_deriving.6.1.1 fmt.0.11.0 yojson.3.0.0 cmdliner.2.1.1

eval $(opam env)
