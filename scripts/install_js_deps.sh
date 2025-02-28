#!/bin/bash

# exit when any command fails
set -e

opam pin -y js_of_ocaml 6.2.0

echo "You should also install node in order to run the tests."
echo "Tests are run with dune build @runjstest"
