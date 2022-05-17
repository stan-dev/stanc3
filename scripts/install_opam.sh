#!/bin/bash

# exit when any command fails
set -e

# The following command installs the latest opam on *nix systems.
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# Initialize opam
opam init --bare --disable-sandboxing

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)
