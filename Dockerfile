FROM ubuntu:bionic
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev

COPY ./scripts/*.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"
RUN bash -x install_dev_deps.sh

