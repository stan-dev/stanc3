#!/bin/sh
# Delete expected files that no rule in their folder's dune.inc diffs against,
# i.e. whose model was removed. Run by make update-rules after the dune.inc
# files are promoted.
set -eu
cd "$(dirname "$0")"
find . -maxdepth 7 -path '*/expected_*/dune.inc' | while read -r inc; do
  dir=$(dirname "$inc")
  kind=$(basename "$dir")
  for file in "$dir"/*; do
    name=$(basename "$file")
    [ "$name" = dune.inc ] && continue
    if ! grep -qF "(diff $kind/$name " "$inc"; then
      echo "Removing $file: no model in $inc produces it"
      rm -- "$file"
    fi
  done
done
