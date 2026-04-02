#!/bin/bash

bash -x ./install_ci_deps.sh
# Useful packages for developer assistance
opam install -y merlin ocaml-lsp-server utop patdiff
