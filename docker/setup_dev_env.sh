#!/bin/bash
# The following command installs the latest opam on *nix systems.
sh -x <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# Once opam is installed, it has to be set up with these commands.
opam init $1
opam config setup -a

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install merlin utop ocp-indent patdiff ocamlformat menhir

# The following looks at what packages we're missing to build stanc.exe
# and tries to install them with opam.
`dune external-lib-deps --missing stanc.exe 2>&1 | tail -n1 | cut -c 12-`
