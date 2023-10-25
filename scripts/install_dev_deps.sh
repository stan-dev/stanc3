#!/bin/bash

# Merlin, utop, ocp-indent, ocamlformat, and patdiff are all for developer assistance
opam pin -y ocamlformat 0.26.1 --no-action
opam install -y ocamlformat.0.26.1 bisect_ppx landmarks-ppx merlin ocaml-lsp-server utop ocp-indent patdiff odoc
