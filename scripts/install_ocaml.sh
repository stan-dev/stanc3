#!/bin/bash

# exit when any command fails
set -e

# get switch name as argument
opam_switch_name=$1

# Create and switch to 4.14.1
# switch can only be created if it doesn't already exist
(opam switch "$opam_switch_name" 2> /dev/null && echo "Opam switch '$opam_switch_name' already found, leaving it be") || opam switch create "$opam_switch_name" ocaml-base-compiler.4.14.1 -y

opam switch "$opam_switch_name"

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)
