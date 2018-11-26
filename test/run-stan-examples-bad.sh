#!/bin/bash
shopt -s globstar

for filename in examples-bad/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe "$filename" ;
done  &> "stan-examples-bad-out.log" ;