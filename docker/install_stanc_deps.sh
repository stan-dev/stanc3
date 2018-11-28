#!/bin/bash
# The following looks at what packages we're missing to build stanc.exe
# and tries to install them with opam.
install_missing=$(dune external-lib-deps --missing stanc.exe 2>&1 | tail -n1 | cut -c 12-)
yes | $install_missing
