#!/bin/bash

# exit when any command fails
set -e

# JSOO versions higher than this break QuickJS (labelled break statements)
# Please check compatibility before any updates!
# https://github.com/stan-dev/stanc3/issues/1425
# https://github.com/bellard/quickjs/issues/275
opam pin -y js_of_ocaml 5.4.0

echo "You should also install node in order to run the tests."
echo "Tests are run with dune build @runjstest"
