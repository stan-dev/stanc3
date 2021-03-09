#!/bin/bash

# The following command installs the latest opam on *nix systems.
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# Initialize opam
opam init $1

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)

# Create and switch to 4.07.0
opam switch create 4.12.0
opam switch 4.12.0

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)