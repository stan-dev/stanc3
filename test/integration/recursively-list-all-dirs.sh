
for filename in $(find -type d -printf '%d\t%P\n' | sort -r -nk1 | cut -f2-) ; do
printf "(glob_files " ;
printf $filename ;
printf "/*.stan)\n"
done;