# Identify the sha256 tag of desired docker image architecture and name of QEMU architecture
if [ $1 = "mips64el" ]; then
  DOCK_ARCH="mips64le"
  DOCK_VARIANT=""
  QEMU_ARCH="mips64el"
elif [ $1 = "arm64" ]; then
  DOCK_ARCH="arm64"
  DOCK_VARIANT=""
  QEMU_ARCH="aarch64"
elif [ $1 = "ppc64el" ]; then
  DOCK_ARCH="ppc64le"
  DOCK_VARIANT=""
  QEMU_ARCH="ppc64le"
elif [ $1 = "armhf" ]; then
  DOCK_ARCH="arm"
  DOCK_VARIANT="v7"
  QEMU_ARCH="arm"
elif [ $1 = "armel" ]; then
  DOCK_ARCH="arm"
  DOCK_VARIANT="v6"
  QEMU_ARCH="arm"
elif [ $1 = "s390x" ]; then
  DOCK_ARCH="s390x"
  DOCK_VARIANT=""
  QEMU_ARCH="s390x"
fi

SHA=$(skopeo inspect --raw docker://andrjohns/stanc3-building:latest | jq '.manifests | .[] | select(.platform.architecture==env.DOCK_ARCH and .platform.variant==(if env.DOCK_VARIANT == "" then null else env.DOCK_VARIANT end)).digest')

# Download QEMU translation binary for desired architecture
docker create --name dummy multiarch/qemu-user-static:x86_64-$QEMU_ARCH bash
docker cp dummy:/usr/bin/qemu-$QEMU_ARCH-static qemu-$QEMU_ARCH-static
docker rm -f dummy

# Register QEMU translation binaries
docker run --rm --privileged multiarch/qemu-user-static --reset

# Run docker, inheriting mounted volumes from sibling container (including stanc3 directory)
docker run --volumes-from=$(docker ps -q):rw -v $(pwd)/qemu-$QEMU_ARCH-static:/usr/bin/qemu-$QEMU_ARCH-static andrjohns/stanc3-building:latest@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

