# Base image
FROM alpine

#Set our distro_style
LABEL distro_style="apk"

#Install os dependencies
RUN apk update && DEBIAN_FRONTEND=noninteractive apk --no-cache add build-base bzip2 git tar curl ca-certificates openssl m4 bash docker jq git-subtree opam ocaml && \
    rm -rf /var/lib/apt/lists/*

# Add a recent version of the Skopeo package, which is used for looking up the correct multiarch docker image
# http://dl-cdn.alpinelinux.org/alpine/v3.14/community/aarch64/containers-common-0.38.11-r0.apk
RUN curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/containers-common-0.52.0-r0.apk -o cont.apk && \
    curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/skopeo-1.12.0-r4.apk -o skopeo.apk && \
    apk add cont.apk && \
    apk add skopeo.apk

#Init opam, create and switch to 4.14.0, update shell environment
RUN opam init --disable-sandboxing --bare -y
RUN opam switch create 4.14.0
RUN opam switch 4.14.0
RUN eval $(opam env)

RUN opam repo add internet https://opam.ocaml.org

COPY ./scripts/install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh

RUN opam install odoc -y

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]