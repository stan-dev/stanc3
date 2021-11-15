# Architecture naming isn't consistent between QEMU and Docker, so lookup correct naming
if [ $1 = "mips64el" ]; then
  export DOCK_ARCH="mips64le"
  export DOCK_VARIANT=""
elif [ $1 = "arm64" ]; then
  export DOCK_ARCH="arm64"
  export DOCK_VARIANT=""
elif [ $1 = "ppc64el" ]; then
  export DOCK_ARCH="ppc64le"
  export DOCK_VARIANT=""
elif [ $1 = "armhf" ]; then
  export DOCK_ARCH="arm"
  export DOCK_VARIANT="v7"
elif [ $1 = "armel" ]; then
  export DOCK_ARCH="arm"
  export DOCK_VARIANT="v6"
elif [ $1 = "s390x" ]; then
  export DOCK_ARCH="s390x"
  export DOCK_VARIANT=""
fi

# Lookup the sha256 hash for the specified architecture and variant (e.g., v7 for armhf) and strip the enclosing quotations
SHA=$(skopeo inspect --raw docker://stanorg/stanc3:multiarch | jq '.manifests | .[] | select(.platform.architecture==env.DOCK_ARCH and .platform.variant==(if env.DOCK_VARIANT == "" then null else env.DOCK_VARIANT end)).digest' | tr -d '"')

# Register QEMU translation binaries
docker run --rm --privileged multiarch/qemu-user-static --reset

# Run docker, inheriting mounted volumes from sibling container (including stanc3 directory), and build stanc3
docker run --volumes-from=$(docker ps -aqf "ancestor=stanorg/stanc3:static"):rw stanorg/stanc3:multiarch@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

# Update ownership of build folders
chown -R opam: _build
chown -R opam: src
chown -R opam: test
