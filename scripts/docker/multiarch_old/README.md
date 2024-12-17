# Using the multiarch Dockerfile

This Dockerfile is used in conjunction with the Docker `buildx` feature to easily build the same image for multiple linux architectures. This approach uses QEMU to emulate the target architecture during building.

### Install QEMU support:

To download the necessary QEMU binaries and register their use, you can run the following Docker image:

```
docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
```

Note that this will need to be re-run after a system reboot

### Create & Initialise Buildx Builder:

Create the buildx builder in docker:

```
docker buildx create --name stanc3_builder
docker buildx use stanc3_builder
```

Check that the newly-created builder supports multiple architectures:

```
docker buildx inspect --bootstrap
[+] Building 4.4s (1/1) FINISHED                                            
 => [internal] booting buildkit                                        4.4s
 => => pulling image moby/buildkit:buildx-stable-1                     3.4s
 => => creating container buildx_buildkit_stanc3_builder0              1.0s
Name:   stanc3_builder
Driver: docker-container

Nodes:
Name:      stanc3_builder0
Endpoint:  unix:///var/run/docker.sock
Status:    running
Platforms: linux/amd64, linux/arm64, linux/riscv64, linux/ppc64le, linux/s390x, linux/386, linux/mips64le, linux/mips64, linux/arm/v7, linux/arm/v6
```

### Build & Push Docker Images

Call `buildx` with the list of target architectures and target repository (note this requires calling `docker login` first)

```
docker buildx build -t stanorg/stanc3:multiarchfi --platform linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le,linux/mips64le,linux/s390x --build-arg PUID=990 --progress=plain --build-arg PGID=986 --push .
docker buildx build -t stanorg/stanc3:multiarchfi --platform linux/mips64le --build-arg PUID=990 --progress=plain --no-cache --build-arg PGID=986 --push .
docker buildx build -t stanorg/stanc3:multiarchfi --platform linux/mips64le --build-arg PUID=990 --progress=plain --build-arg PGID=986 --push .
```