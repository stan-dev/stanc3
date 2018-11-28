#!/bin/bash
# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install -y merlin utop ocp-indent patdiff ocamlformat menhir
