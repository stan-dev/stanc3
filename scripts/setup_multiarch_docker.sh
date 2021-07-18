apt update 
apt install opam bzip2 git tar curl ca-certificates openssl m4 bash -y

opam init --disable-sandboxing -y
opam switch create 4.07.0
opam switch 4.07.0
eval $(opam env)
opam repo add internet https://opam.ocaml.org

cd /stanc3
bash -x scripts/install_build_deps.sh
dune build @install --profile static
exit