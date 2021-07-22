echo "
  FROM multiarch/debian-debootstrap:$1-bullseye

  RUN apt-get update
  RUN apt-get install opam bzip2 git tar curl ca-certificates openssl m4 bash -y

  RUN opam init --disable-sandboxing -y
  RUN opam switch create 4.07.0
  RUN opam switch 4.07.0
  RUN eval \$(opam env)
  RUN opam repo add internet https://opam.ocaml.org
" > $1-dockerfile

echo "
cd $(pwd)

bash -x scripts/install_build_deps.sh
dune build @install --profile static
" > scripts/build_stanc3.sh

docker run --rm --privileged multiarch/qemu-user-static:register --reset
docker build -t $1-build - < $1-dockerfile
docker run --volumes-from=$(docker ps -q):rw $1-build /bin/bash -c "bash -x $(pwd)/scripts/build_stanc3.sh"

rm $1-dockerfile
rm scripts/build_stanc3.sh