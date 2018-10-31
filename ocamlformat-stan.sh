for foldername in folder/*; do cd "src/stan"; for filename in *.ml; do ocamlformat -i "$filename"; done; cd ../..; done
# TODO: Find a way to auto-format the .mll and .mly files as well.