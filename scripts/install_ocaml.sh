#!/bin/bash
# The following command installs the latest opam on *nix systems.
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
opam init $1
eval $(opam env)
opam switch create 4.07.0
