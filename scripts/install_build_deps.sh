#!/bin/bash

# Menhir is our parsing library and annoyingly its module name does not match
# its library name, so we install it manually here.

opam pin -y dune 2.8.2

opam install -y core.v0.14.1 menhir.20210310 ppx_deriving.5.2.1 fmt.0.8.9 yojson.1.7.0
