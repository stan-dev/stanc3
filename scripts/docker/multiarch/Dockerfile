# Base image
FROM debian:buster-20220622-slim

USER root

ARG PUID
ARG PGID

# Update repositories and install OS deps
RUN apt-get update
RUN apt-get install opam curl bzip2 git tar curl ca-certificates openssl m4 bash -y

RUN update-ca-certificates -f

# Setup jenkins uid/gid
RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

WORKDIR /qemu-setup

# Identify architecture being built for and cache
RUN echo $(dpkg --print-architecture) > ./arch

# Translate dpkg architecture naming to QEMU architecture
RUN if [ $(cat ./arch) = "arm64" ]; then \
      QEMU_ARCH="aarch64"; \
    elif [ $(cat ./arch) = "ppc64el" ]; then \
      QEMU_ARCH="ppc64le"; \
    elif [ $(cat ./arch) = "armhf" ]; then \
      QEMU_ARCH="arm"; \
    elif [ $(cat ./arch) = "armel" ]; then \
      QEMU_ARCH="arm"; \
    else \
      QEMU_ARCH=$(cat ./arch); \
    fi && \
    echo $QEMU_ARCH > ./qarch

# Download needed QEMU binary and place in /usr/bin
RUN curl -L https://github.com/multiarch/qemu-user-static/releases/download/v6.0.0-2/x86_64_qemu-$(cat ./qarch)-static.tar.gz | tar -xzf - -C /usr/bin

RUN chown jenkins /usr/bin/qemu-$(cat ./qarch)-static

USER jenkins
WORKDIR /home/jenkins

RUN opam init --disable-sandboxing --bare -y
RUN eval $(opam env) && opam update

# Native-code compilation not available on MIPS, fall back to bytecode
RUN if [ "$(cat /qemu-setup/arch)" = "mips64el" ]; then \
    opam switch create 4.14.1 --packages=ocaml-variants.4.14.1+options,ocaml-option-bytecode-only && opam switch 4.14.1 && opam pin num https://github.com/ocaml/num.git -y; \
  else \
    opam switch create 4.14.1 && opam switch 4.14.1; \
  fi

RUN eval $(opam env) && opam repo add internet https://opam.ocaml.org

RUN eval $(opam env) && opam install -y dune
RUN eval $(opam env) && opam update && opam upgrade
RUN eval $(opam env) && opam install -y core.v0.16.0
RUN eval $(opam env) && opam install -y menhir.20230608
RUN eval $(opam env) && opam install -y ppx_deriving.5.2.1
RUN eval $(opam env) && opam install -y fmt.0.9.0
RUN eval $(opam env) && opam install -y yojson.2.1.0
RUN eval $(opam env)