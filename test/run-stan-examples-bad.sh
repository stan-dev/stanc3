dir=`pwd`
for filename in $(find $dir/examples-bad -name '*.stan'); do
  printf "\n\n $filename \n ---------\n"; $dir/../_build/default/stanc.exe "$filename" ;
done  &> $dir/"stan-examples-bad-out.log" ;