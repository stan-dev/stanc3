dir=`pwd`
for filename in $(find $dir/examples-good -name '*.stan'); do
  printf "\n\n $filename \n ---------\n"; $dir/../_build/default/stanc.exe --auto-format "$filename" ;
done  &> $dir/"stan-examples-good-pretty-printed.log" ;