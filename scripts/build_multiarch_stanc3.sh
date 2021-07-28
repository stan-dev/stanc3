# Identify 
if [ $1 = "mips64el" ]; then
  SHA="5a13b9b407adebae3e3c32b8a1f68d1aa6e8121769e24d21108f61edaaebc09e"
else
  SHA=$1
fi

docker run --rm --privileged multiarch/qemu-user-static:register --reset
docker run --volumes-from=$(docker ps -q):rw andrjohns/stanc3-building:latest@sha256:$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

