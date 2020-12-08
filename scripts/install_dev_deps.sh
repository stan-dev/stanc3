#!/bin/bash

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
opam pin -y ocamlformat 0.8
opam pin -y merlin 3.4.1
opam install -y ocamlformat.0.8 merlin.3.4.1 utop ocp-indent patdiff