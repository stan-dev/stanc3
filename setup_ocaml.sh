sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

opam init
opam config setup -a

opam install merlin utop ocp-indent
