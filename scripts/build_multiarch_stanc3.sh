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

id -u
id -un
id -G
id -Gn
pwd

# Run docker, inheriting mounted volumes from sibling container (including stanc3 directory), and build stanc3 #
# docker run -u 990:986 --group-add=987 --group-add=988 --volumes-from=$(docker ps -qf "ancestor=stanorg/stanc3:staticfi"):rw stanorg/stanc3:multiarchfi@$SHA /bin/bash -c "cd $(pwd) && eval \$(opam env) && dune build @install --profile static"
# docker run -t -d -u 990:986 --entrypoint= -w /home/jenkins/workspace/Stan_Stanc3_PR-1087 -v /home/jenkins/workspace/Stan_Stanc3_PR-1087:/home/jenkins/workspace/Stan_Stanc3_PR-1087:rw,z -v /home/jenkins/workspace/Stan_Stanc3_PR-1087@tmp:/home/jenkins/workspace/Stan_Stanc3_PR-1087@tmp:rw,z
# -w $(pwd) -v $(pwd):$(pwd):rw
# docker run -u 990:986 --group-add=987 --group-add=988 stanorg/stanc3:multiarch@$SHA
docker run -u 990:986 --group-add=987 --group-add=988 stanorg/stanc3:multiarch@$SHA /bin/bash -c "echo hi"

# Update ownership of build folders
#chown -R opam: _build
#chown -R opam: src
#chown -R opam: test

# docker buildx build -t stanorg/stanc3:multiarchfi --platform linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le,linux/mips64le,linux/s390x --no-cache --progress=plain --build-arg PUID=990 --build-arg PGID=986 --build-arg DOCKER_GID=987 --build-arg DOCKERROOT_GID=988 --push .

# docker run -v $(pwd):/home/jenkins stanorg/stanc3:multiarchfi@sha256:f5098ad99b4245da5f811e329af326bf241e46c67789e16906da7364b3fb9db1 /bin/bash -c 'which opam && eval $(opam env) && dune build @install --profile static'

# docker run -e PATH='/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' stanorg/stanc3:multiarchfi@sha256:9c65132ebb1b920a68a171fb07427d7e4992ee438b377b9f2ab71a29b01cbc7d /bin/bash -c "echo hi"

#docker run stanorg/stanc3:multiarchfi@sha256:9c65132ebb1b920a68a171fb07427d7e4992ee438b377b9f2ab71a29b01cbc7d qemu-mips64el-static
#docker run stanorg/stanc3:multiarchfi@sha256:9c65132ebb1b920a68a171fb07427d7e4992ee438b377b9f2ab71a29b01cbc7d /bin/bash