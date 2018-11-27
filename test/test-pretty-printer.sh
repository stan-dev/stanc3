for filename in $(find examples-good -name '*.stan'); do
  printf "\n\n $filename \n ---------\n"; ./../_build/default/stanc.exe --auto-format "$filename" ;
done  &> "stan-examples-good-pretty-printed.log" ;