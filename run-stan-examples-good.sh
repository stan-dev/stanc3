#!/bin/bash
shopt -s globstar

for filename in src/stan/examples-good/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./stan.native "$filename" ;
done  &> "stan-examples-good-out.log" ;