# Identify 
if [ $1 = "mips64el" ]; then
  SHA="sha256:f674ab3524ccdc9af392e50c3fa0be937c0bb8df54be1b4b671460197d3efebd"
  ARCH="mips64el"
elif [ $1 = "arm64" ]; then
  SHA="sha256:d9311875f5fcd5f9b2221efb9d8a10ea8a159ca0e30cc60f04a0333ba14a4371"
  ARCH="aarch64"
elif [ $1 = "ppc64el" ]; then
  SHA="sha256:cce409af4067aa8f6825dc07c04a7038c025a4a9b7b71355ce04e066f2c65b07"
  ARCH="ppc64le"
elif [ $1 = "armhf" ]; then
  SHA="sha256:55d95cae75451deb2dde29437edea9375c6334b4fb05abd63f1e04006101cf69"
  ARCH="arm"
elif [ $1 = "armel" ]; then
  SHA="sha256:6d5de11aaafa16126eae8bcb314dfb9d50fb7052c2a6d116e0de800699363cc6"
  ARCH="arm"
elif [ $1 = "s390x" ]; then
  SHA="sha256:a323d161c8addae675c1af740e6ee62ddae76c8ac7e9e1100b834aab703a78ad"
  ARCH="s390x"
else
  SHA=$1
fi

docker create --name dummy multiarch/qemu-user-static:x86_64-$ARCH bash
docker cp dummy:/usr/bin/qemu-$ARCH-static qemu-$ARCH-static
docker rm -f dummy

docker run --rm --privileged multiarch/qemu-user-static --reset
docker run --volumes-from=$(docker ps -q):rw -v $(pwd)/qemu-$ARCH-static:/usr/bin/qemu-$ARCH-static andrjohns/stanc3-building:latest@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

