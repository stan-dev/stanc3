#Use Official Docker Images for OCAML/OPAM
#https://github.com/ocaml/infrastructure/wiki/Containers
FROM ocaml/opam2:alpine

#Switch to root user so we can install apk packages
USER root

#Set our distro_style
LABEL distro_style="apk"

#Install os dependencies
RUN apk update && apk add build-base bzip2 git tar curl ca-certificates openssl m4 bash

#Switch back to the normal user
USER opam

#Copy script & Install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Update the current shell environment
RUN eval $(opam env)

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]