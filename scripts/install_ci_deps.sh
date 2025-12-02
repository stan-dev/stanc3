#!/bin/bash

# ocamlformat, bisect_ppx, and odoc are needed for CI and useful for developers
opam pin -y ocamlformat 0.28.1 --no-action
opam pin -y bisect_ppx https://github.com/aantron/bisect_ppx.git#2d8dffbbfc0c431a37319d4d9a143836c9ec542e --no-action
opam install -y ocamlformat bisect_ppx odoc
