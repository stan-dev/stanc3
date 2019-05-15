#!/bin/bash
# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.
opam install -y dune.1.8.2 core_kernel.v0.11.1 menhir.20181113 ppx_deriving.4.2.1 fmt.0.8.6
