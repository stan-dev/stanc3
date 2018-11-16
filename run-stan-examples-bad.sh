#!/bin/bash
shopt -s globstar

for filename in src/stan/examples-bad/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./stan.native "$filename" ;
done  &> "stan-examples-bad-out.log" ;