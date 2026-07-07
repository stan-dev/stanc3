#!/bin/bash

# exit when any command fails
set -e

# ocamlformat, bisect_ppx, and odoc are needed for CI and useful for developers
opam pin -y ocamlformat 0.28.1 --no-action
opam pin -y bisect_ppx https://github.com/aantron/bisect_ppx.git#7061d643ff492b0045796357ee6917ded21fb1f0 --no-action
opam install -y ocamlformat bisect_ppx odoc sherlodoc
