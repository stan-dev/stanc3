#!/bin/bash
shopt -s globstar

for filename in examples-good/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe "$filename" ;
done  &> "stan-examples-good-out.log" ;