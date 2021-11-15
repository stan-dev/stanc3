#!/bin/bash

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
opam pin -y ocamlformat 0.19.0
opam install -y ocamlformat.0.19.0 merlin ocaml-lsp-server utop ocp-indent patdiff odoc
