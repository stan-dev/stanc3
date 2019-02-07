#!/bin/bash
bash -x ./docker/install_ocaml.sh
bash -x ./docker/install_build_deps.sh
bash -x ./docker/install_dev_deps.sh

# The following looks at what packages we're missing to build stanc.exe
# and tries to install them with opam.
install_missing=$(dune external-lib-deps --missing stanc.exe 2>&1 | tail -n1 | cut -c 12-)
echo "Trying to install missing deps for stanc with $install_missing"
$install_missing
