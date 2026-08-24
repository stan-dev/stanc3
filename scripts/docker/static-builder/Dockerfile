FROM alpine:3.21
LABEL distro_style="apk"
#Switch to root user so we can install apk packages
USER root
RUN apk update && apk add --no-cache opam build-base bzip2 git tar curl ca-certificates openssl m4 bash

# Switch back to a normal user
RUN adduser -D jenkins
USER jenkins

# Init opam, create and switch to 5.5.0, update shell environment
RUN opam init --disable-sandboxing --bare -y
RUN opam switch create 5.5.0 ocaml-base-compiler.5.5.0 -y
RUN opam switch 5.5.0
RUN eval $(opam env)

COPY install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh
