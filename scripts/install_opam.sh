#!/bin/bash

# exit when any command fails
set -e

# The following command installs the latest opam on *nix systems.
sh <(curl -sL https://opam.ocaml.org/install.sh)

# Initialize opam
opam init --bare --disable-sandboxing

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)
