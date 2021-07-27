# Identify 
if [ $1 = "armhf" ]; then
  $SHA="06aabdc4092100d08ba26d986acb27931b43255fdec9706bf48896e41c3db221"
else
  $SHA=$1
fi

echo "
  cd $(pwd)

  eval \$(opam env)
  dune build @install --profile static
" > scripts/build_stanc3.sh

docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
docker run --volumes-from=$(docker ps -q):rw andrjohns/stanc3-building:latest@sha256:$SHA /bin/bash -x $(pwd)/scripts/build_stanc3.sh

rm scripts/build_stanc3.sh
