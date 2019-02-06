#!/bin/bash
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install -y core_kernel menhir ppx_deriving fmt
