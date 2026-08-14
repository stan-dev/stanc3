#!/bin/bash

# exit when any command fails
set -e

# ocamlformat, bisect_ppx, and odoc are needed for CI and useful for developers
opam pin -y ocamlformat 0.29.0 --no-action

# dev pin for newer ppxlib compatability
opam pin -y bisect_ppx https://github.com/aantron/bisect_ppx.git#7061d643ff492b0045796357ee6917ded21fb1f0 --no-action
# dev pin for yojson 3.0 compatability
opam pin add -y linol     https://github.com/c-cube/linol.git#6ad59959e46bdb87687046a86b752b4fc1518f2a --no-action
opam pin add -y linol-lwt https://github.com/c-cube/linol.git#6ad59959e46bdb87687046a86b752b4fc1518f2a --no-action

opam install -y ocamlformat bisect_ppx odoc sherlodoc menhirformat ppx_sexp_value
