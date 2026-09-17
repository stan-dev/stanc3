FROM ubuntu:noble

USER root

# Set the opam root
ENV OPAMROOT=/usr/local/opam

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    build-essential ca-certificates curl gcc git m4 \
    mingw-w64 pkg-config python3 unzip zip wget nodejs gh

# Codecov uploader
RUN wget https://uploader.codecov.io/v0.8.0/linux/codecov -O /bin/codecov
RUN chmod +x /bin/codecov

# opam
RUN bash -c "printf '\n' | sh <(curl -fsSL https://opam.ocaml.org/install.sh)"

USER ubuntu
WORKDIR /home/ubuntu
ENV OPAMROOT=/home/ubuntu/.opam
ENV DEBIAN_FRONTEND=noninteractive

# opam setup
RUN opam init --bare --disable-sandboxing -y

# Install and initialize ocaml
COPY install_ocaml.sh ./
RUN opam update; printf "\n" | bash -x install_ocaml.sh "stanc"

# Install build dependencies
COPY install_build_deps_windows.sh ./
RUN opam update; bash -x install_build_deps_windows.sh

# Install Javascript dev environment (js_of_ocaml 6.4.1)
COPY install_js_deps.sh ./
RUN opam update; bash -x install_js_deps.sh

# Install dev dependencies
COPY install_ci_deps.sh ./
RUN opam update; bash -x install_ci_deps.sh
