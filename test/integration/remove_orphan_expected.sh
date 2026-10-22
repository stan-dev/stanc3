#!/bin/bash
# Delete expected files that have no source file
set -eu
cd "$(dirname "$0")"
removed=0

find . -maxdepth 7 -path '*/expected_*' -type d | while read -r dir; do
  parent_dir=$(dirname "$dir")
  for file in "$dir"/*; do
    base="$(basename $file)"
    name="${base%%.*}"

    if [ ! -f "$parent_dir/$name.stan" ]; then
      removed=1
      rm -f "$file"
    fi
  done
done

exit $removed
