sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

opam init
opam config setup -a

opam install merlin utop ocp-indent menhir
`dune external-lib-deps --missing ocaml/stanc.exe 2>&1 | tail -n1 | cut -c 12-`
