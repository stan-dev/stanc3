# Pull the ubuntu:jammy base image
FROM ubuntu:jammy

USER root

ARG PUID
ARG PGID

# Set the opam root
ENV OPAMROOT /usr/local/opam

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl ca-certificates rsync git build-essential m4 unzip pkg-config\
    libpcre3-dev python3 python3-pip sudo
RUN curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -
RUN apt-get update -qq && apt-get install -y --no-install-recommends nodejs

RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

USER jenkins
WORKDIR /home/jenkins

#Copy our script and install ocaml + init
COPY ./scripts/install_opam.sh ./
RUN printf "\n" | bash -x install_opam.sh
RUN opam update

# Install and initialize ocaml
COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "stanc"

# Install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

# Install dev dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

# Install Javascript dev environment (js_of_ocaml 5.9.1)
COPY ./scripts/install_js_deps.sh ./
RUN bash -x install_js_deps.sh

# Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
