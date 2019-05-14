#Pull the ubuntu:bionic image
FROM ubuntu:bionic

#Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev

#Copy our script and install ocaml + init
COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"

#Copy our script and install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Copy our script and install dev dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

#this is just used for end to end tests, maybe temporary
RUN apt-get install -y --no-install-recommends python

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]