opam pin -y js_of_ocaml-ppx 3.11.0
opam install -y js_of_ocaml-ppx

echo "You should also install node in order to run the tests."
echo "Tests are run with dune build @runjstest"
