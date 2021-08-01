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

# Need to use the sha256 tag to force docker to download an architecture that differs from the host
# We'll use the skopeo package to lookup the most recent tag, but need to manually install a more recent version
curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/containers-common-0.38.11-r0.apk -o cont.apk
curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/skopeo-1.3.1-r0.apk -o skopeo.apk

apk add cont.apk
apk add skopeo.apk

# Lookup the sha256 hash for the specified architecture and variant (e.g., v7 for armhf) and strip the enclosing quotations
SHA=$(skopeo inspect --raw docker://andrjohns/stanc3-building:latest | jq '.manifests | .[] | select(.platform.architecture==env.DOCK_ARCH and .platform.variant==(if env.DOCK_VARIANT == "" then null else env.DOCK_VARIANT end)).digest' | tr -d '"')

# Register QEMU translation binaries
docker run --rm --privileged multiarch/qemu-user-static --reset

# Run docker, inheriting mounted volumes from sibling container (including stanc3 directory), and build stanc3
docker run --volumes-from=$(docker ps -q):rw andrjohns/stanc3-building:latest@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"

# Update ownership of build folders
chown -R opam: _build
chown -R opam: src
chown -R opam: test
