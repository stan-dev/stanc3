#!/bin/bash
shopt -s globstar

for filename in examples-good/**/*.stan; do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe --auto-format "$filename" ;
done  &> "stan-examples-good-pretty-printed.log" ;