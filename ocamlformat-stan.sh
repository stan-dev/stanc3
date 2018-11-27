#!/bin/bash
shopt -s nullglob

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

for filename in $(find $dir/lib -name '*.ml' -or -name '*.mli'); do
  ocamlformat -i "$filename";
done;
for filename in  $dir/*.ml; do
  ocamlformat -i "$filename";
done;
for filename in  $dir/*.mli; do
  ocamlformat -i "$filename";
done;
# TODO: Find a way to auto-format the .mll and .mly files as well.