FROM debian:buster-slim

RUN apt-get update
RUN apt-get install opam curl bzip2 git tar curl ca-certificates openssl m4 bash -y

RUN update-ca-certificates -f

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

RUN opam init --disable-sandboxing -y
RUN eval $(opam env)

# Native-code compilation not available on MIPS, fall back to bytecode
RUN if [ $(cat ./arch) = "mips64el" ]; then \
    opam switch create 4.07.0+bytecode-only && opam switch 4.07.0+bytecode-only; \
  else \
    opam switch create 4.07.0 && opam switch 4.07.0; \
  fi
  
RUN eval $(opam env) && opam repo add internet https://opam.ocaml.org

# Bytecode installation not available using older menhir version with opam,
#   have to manually compile menhir with necessary flags
RUN if [ $(cat ./arch) = "mips64el" ]; then \
    curl https://gitlab.inria.fr/fpottier/menhir/-/archive/20181113/menhir-20181113.tar.gz --output menhir-20181113.tar.gz && \
    tar -xf menhir-20181113.tar.gz && cd menhir-20181113/ && \
    eval $(opam env) && opam install -y ocamlbuild ocamlfind && \
    make -f Makefile PREFIX=/root/.opam/4.07.0+bytecode-only USE_OCAMLFIND=true docdir=/root/.opam/4.07.0+bytecode-only/doc/menhir libdir=/root/.opam/4.07.0+bytecode-only/lib/menhir mandir=/root/.opam/4.07.0+bytecode-only/man/man1 TARGET=byte all && \
    make -f Makefile PREFIX=/root/.opam/4.07.0+bytecode-only USE_OCAMLFIND=true docdir=/root/.opam/4.07.0+bytecode-only/doc/menhir libdir=/root/.opam/4.07.0+bytecode-only/lib/menhir mandir=/root/.opam/4.07.0+bytecode-only/man/man1 TARGET=byte install; \
    fi

# If menhir already manually installed, then remove install command from bash script
RUN if [ $(cat ./arch) = "mips64el" ]; then \
    curl https://raw.githubusercontent.com/stan-dev/stanc3/master/scripts/install_build_deps.sh | \
    sed 's/menhir.20181113//' | bash; \
    else \
    curl https://raw.githubusercontent.com/stan-dev/stanc3/master/scripts/install_build_deps.sh | bash; \
    fi

# Cleanup
RUN rm ./arch && rm ./qarch
