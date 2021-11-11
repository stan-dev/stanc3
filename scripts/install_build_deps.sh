#!/bin/bash

# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.

opam pin -y dune 2.8.4

opam install -y core_kernel.v0.14.2 menhir.20210929 ppx_deriving.5.2.1 fmt.0.8.8 yojson.1.7.0
