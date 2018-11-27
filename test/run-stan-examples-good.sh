for filename in $(find examples-good -name '*.stan'); do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe "$filename" ;
done  &> "stan-examples-good-out.log" ;