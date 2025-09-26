FROM alpine:3.21
LABEL distro_style="apk"
#Switch to root user so we can install apk packages
USER root

ARG PUID
ARG PGID

RUN addgroup -g ${PGID} jenkins && \
    adduser -D -u ${PUID} -G jenkins jenkins

RUN apk update && apk add --no-cache opam build-base bzip2 git tar curl ca-certificates openssl m4 bash

# Switch back to the normal user
USER jenkins

# Init opam, create and switch to 4.14.0, update shell environment
RUN opam init --disable-sandboxing --bare -y
RUN opam switch create 4.14.1
RUN opam switch 4.14.1
RUN eval $(opam env)

COPY ./scripts/install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh

RUN opam install odoc sherlodoc -y
