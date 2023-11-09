#Pull the ubuntu:bionic image
FROM ubuntu:bionic

USER root

ARG PUID
ARG PGID

# Set the opam root
ENV OPAMROOT /usr/local/opam

RUN apt-get update -qq && apt-get install -y --no-install-recommends sudo curl ca-certificates rsync git build-essential m4 unzip pkg-config libpcre3-dev mingw-w64 gcc wget gawk vim nano time

RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

USER jenkins
WORKDIR /home/jenkins

ENV DEBIAN_FRONTEND noninteractive
ENV OPAMROOT /usr/local/opam

#Copy our script and install ocaml + init
COPY ./scripts/install_opam.sh ./
RUN printf "\n" | bash -x install_opam.sh

#Copy our script and install ocaml + init
COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "stanc"

#Copy our script and install build dependencies
COPY ./scripts/install_build_deps_windows.sh ./
RUN bash -x install_build_deps_windows.sh

#Copy our script and install dev dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

# Install Javascript dev environment
COPY ./scripts/install_js_deps.sh ./
RUN opam update; bash -x install_js_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
