# Pull the ubuntu:jammy base image
FROM ubuntu:jammy

USER root

ARG PUID
ARG PGID

# Set the opam root
ENV OPAMROOT=/usr/local/opam

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    build-essential ca-certificates curl gawk gcc git libpcre3-dev m4 \
    mingw-w64 nano pkg-config python3 python3-pip rsync sudo time unzip \
    zip wget

# Newer nodejs version
RUN curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -
RUN apt-get update -qq && apt-get install -y --no-install-recommends nodejs

# Codecov uploader
RUN wget https://uploader.codecov.io/v0.8.0/linux/codecov -O /bin/codecov
RUN chmod +x /bin/codecov

# Github CLI
RUN sudo mkdir -p -m 755 /etc/apt/keyrings && wget -qO- https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo tee /etc/apt/keyrings/githubcli-archive-keyring.gpg > /dev/null \
    && sudo chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
    && sudo apt update \
    && sudo apt install gh -y


RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

USER jenkins
WORKDIR /home/jenkins

ENV DEBIAN_FRONTEND=noninteractive

# OCaml setup

# Copy our script and install ocaml + init
COPY ./scripts/install_opam.sh ./
RUN printf "\n" | bash -x install_opam.sh

# Install and initialize ocaml
COPY ./scripts/install_ocaml.sh ./
RUN opam update; printf "\n" | bash -x install_ocaml.sh "stanc"

# Install build dependencies
COPY ./scripts/install_build_deps_windows.sh ./
RUN opam update; bash -x install_build_deps_windows.sh

# Install Javascript dev environment (js_of_ocaml 5.9.1)
COPY ./scripts/install_js_deps.sh ./
RUN opam update; bash -x install_js_deps.sh

# Install dev dependencies
COPY ./scripts/install_ci_deps.sh ./
RUN opam update; bash -x install_ci_deps.sh

# Specify our entrypoint
ENTRYPOINT [ "opam", "exec", "--" ]
