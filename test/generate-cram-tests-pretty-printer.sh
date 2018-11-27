dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )" ;
for filename in $(find  $dir/examples-good -name '*.stan' -printf "%P\n"); do
  changepath=$(echo $filename | sed -e 's/[^\/]*/../g')
  changepath=${changepath::-2}
  printf "  \$ \$TESTDIR/$changepath/../../_build/default/stanc.exe --auto-format \"\$TESTDIR/$changepath/$filename\"\n\n" &> $dir/examples-good/${filename}-pretty-print-test.t ; 
done