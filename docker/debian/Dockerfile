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
RUN opam update; bash -x install_build_deps.sh

#Copy our script and install dev dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN opam update; bash -x install_dev_deps.sh

RUN opam update; opam install -y js_of_ocaml-compiler.3.4.0 js_of_ocaml-ppx.3.4.0 js_of_ocaml.3.4.0

# Add Javascript dev environment
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  nodejs
COPY ./scripts/install_js_deps.sh ./
RUN opam update; bash -x install_js_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
