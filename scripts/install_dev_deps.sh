#!/bin/bash

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
opam pin -y ocamlformat 0.8
opam install -y merlin utop ocp-indent patdiff
