dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
for filename in $(find $dir/examples-good -name '*.stan'); do
  $dir/../_build/default/stanc.exe "$filename" ;
done 