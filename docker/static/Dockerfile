#Use Official Docker Images for OCAML/OPAM
#https://github.com/ocaml/infrastructure/wiki/Containers
FROM ocaml/opam2:alpine-3.9-ocaml-4.07

#Switch to root user so we can install apk packages
USER root

#Add opam group

RUN addgroup -g 1000 opam
RUN addgroup -g 1004 jenkins-slave

RUN delgroup opam nogroup

RUN addgroup opam opam
RUN addgroup opam jenkins-slave

#Set our distro_style
LABEL distro_style="apk"

#Install os dependencies
RUN apk update && apk add build-base bzip2 git tar curl ca-certificates openssl m4 bash

# Add a recent version of the Skopeo package, which is used for looking up the correct multiarch docker image
RUN curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/containers-common-0.38.11-r0.apk -o cont.apk && \
    curl https://dl-cdn.alpinelinux.org/alpine/latest-stable/community/x86_64/skopeo-1.3.1-r1.apk -o skopeo.apk && \
    apk add cont.apk && \
    apk add skopeo.apk

#Switch back to the normal user
USER opam

#Init opam, create and switch to 4.07.0, update shell environment
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.07.0
RUN opam switch 4.07.0
RUN eval $(opam env)

RUN opam repo add internet https://opam.ocaml.org

COPY ./scripts/install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
