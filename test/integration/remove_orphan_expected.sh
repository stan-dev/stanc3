#!/bin/bash
# Delete expected files that have no source file
set -eu
cd "$(dirname "$0")"
removed=0

while read -r dir; do
  parent_dir=$(dirname "$dir")
  for file in "$dir"/*; do
    base="$(basename $file)"
    name="${base%%.*}"

    if [ ! -f "$parent_dir/$name.stan" -a ! -f "$parent_dir/$name.stanfunctions" ]; then
        removed=1
        echo "Deleting test file $file, which does not have a matching .stan[functions] source!"
        rm -f "$file"
    fi
  done
done < <(find . -maxdepth 7 -path '*/expected_*' -type d)

exit $removed
