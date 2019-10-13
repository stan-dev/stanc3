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

#Switch back to the normal user
USER opam

#Init opam, create and switch to 4.07.0, update shell environment
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.07.0
RUN opam switch 4.07.0
RUN eval $(opam env)

#Copy script & Install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
