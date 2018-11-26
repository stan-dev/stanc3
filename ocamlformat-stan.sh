#!/bin/bash

shopt -s nullglob
for filename in *.ml; do
  ocamlformat -i "$filename";
done;
for filename in *.mli; do
  ocamlformat -i "$filename";
done;
cd "lib";
for filename in *.ml; do
  ocamlformat -i "$filename";
done;
for filename in *.mli; do
  ocamlformat -i "$filename";
done;
cd ../..;
# TODO: Find a way to auto-format the .mll and .mly files as well.