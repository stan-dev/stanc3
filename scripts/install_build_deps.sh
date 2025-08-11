#!/bin/bash

# exit when any command fails
set -e

eval $(opam env)

opam pin -y core v0.16.0 --no-action

opam install -y dune core.v0.16.0 menhir.20230608 ppx_deriving.5.2.1 fmt.0.11.0 yojson.2.1.0 cmdliner.1.3.0

eval $(opam env)
