#!/bin/bash

bash -x ./install_ocaml.sh
bash -x ./install_build_deps.sh
bash -x ./install_dev_deps.sh

eval $(opam env)

# The following looks at what packages we're missing to build stanc.exe
# and tries to install them with opam.
install_missing=$(dune external-lib-deps --missing src/stanc/stanc.exe 2>&1 | tail -n1 | cut -c 12-)

echo "Trying to install missing deps for stanc with $install_missing"

$install_missing
