#!/bin/bash

# exit when any command fails
set -e

# get switch name as argument
opam_switch_name=$1

# switch can only be created if it doesn't already exist
(opam switch "$opam_switch_name" 2> /dev/null && echo "Opam switch '$opam_switch_name' already found, leaving it be") \
    || opam switch create "$opam_switch_name" ocaml-variants.5.4.0+options ocaml-option-flambda -y

opam switch "$opam_switch_name"

# Have further shell commands be evaluated in the proper opam context.
eval $(opam env)
