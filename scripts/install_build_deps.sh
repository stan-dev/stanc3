#!/bin/bash
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install -y dune.1.8.2 core_kernel menhir ppx_deriving fmt
