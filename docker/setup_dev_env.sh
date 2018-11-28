#!/bin/bash
# The following command installs the latest opam on *nix systems.
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
opam init $1

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install -y merlin utop ocp-indent patdiff ocamlformat menhir

# The following looks at what packages we're missing to build stanc.exe
# and tries to install them with opam.
install_missing=$(dune external-lib-deps --missing stanc.exe 2>&1 | tail -n1 | cut -c 12-)
yes | $install_missing
