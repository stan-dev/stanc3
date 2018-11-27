for filename in $(find examples-bad -name '*.stan'); do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe "$filename" ;
done  &> "stan-examples-bad-out.log" ;