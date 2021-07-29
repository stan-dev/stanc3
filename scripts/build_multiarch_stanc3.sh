# Identify 
if [ $1 = "mips64el" ]; then
  ARCH="linux/mips64le"
elif [ $1 = "arm64" ]; then
  ARCH="linux/arm64"
elif [ $1 = "ppc64el" ]; then
  ARCH="linux/ppc64le"
elif [ $1 = "armhf" ]; then
  ARCH="linux/arm/v7"
elif [ $1 = "armel" ]; then
  ARCH="linux/arm/v6"
else
  ARCH=$1
fi

docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
docker run --volumes-from=$(docker ps -q):rw --platform $ARCH andrjohns/stanc3-building:latest /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

