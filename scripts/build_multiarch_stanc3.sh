
# Architecture naming isn't consistent between QEMU and Docker, so lookup correct naming
if [ $1 = "mips64el" ]; then
  export DOCK_PLATFORM="linux/mips64le"
  export DOCK_ARCH="mips64le"
  export DOCK_VARIANT=""
elif [ $1 = "arm64" ]; then
  export DOCK_PLATFORM="linux/arm64"
  export DOCK_ARCH="arm64"
  export DOCK_VARIANT=""
elif [ $1 = "ppc64el" ]; then
  export DOCK_PLATFORM="linux/ppc64le"
  export DOCK_ARCH="ppc64le"
  export DOCK_VARIANT=""
elif [ $1 = "armhf" ]; then
  export DOCK_PLATFORM="linux/arm/v7"
  export DOCK_ARCH="arm"
  export DOCK_VARIANT="v7"
elif [ $1 = "armel" ]; then
  export DOCK_PLATFORM="linux/arm/v6"
  export DOCK_ARCH="arm"
  export DOCK_VARIANT="v6"
elif [ $1 = "s390x" ]; then
  export DOCK_PLATFORM="linux/s390x"
  export DOCK_ARCH="s390x"
  export DOCK_VARIANT=""
fi

DOCKER_IMAGE_TAG="$2"

# Lookup the sha256 hash for the specified architecture and variant (e.g., v7 for armhf) and strip the enclosing quotations
SHA=$(skopeo inspect --raw docker://stanorg/stanc3:${DOCKER_IMAGE_TAG} | jq '.manifests | .[] | select(.platform.architecture==env.DOCK_ARCH and .platform.variant==(if env.DOCK_VARIANT == "" then null else env.DOCK_VARIANT end)).digest' | tr -d '"')

# Register QEMU translation binaries
docker run --privileged --rm tonistiigi/binfmt --install all

docker run  --platform=${DOCK_PLATFORM} --group-add=987 --group-add=980 --group-add=988 -v $(pwd):$(pwd):rw,z \
  stanorg/stanc3:${DOCKER_IMAGE_TAG}@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune subst && dune clean && dune build @install --profile static --root=."
