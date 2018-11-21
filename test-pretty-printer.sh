#!/bin/bash
shopt -s globstar

for filename in src/stan/examples-good/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./stan.native --auto-format "$filename" ;
done  &> "stan-examples-good-pretty-printed.log" ;