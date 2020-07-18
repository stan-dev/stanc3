#Pull the ubuntu:bionic image
FROM ubuntu:bionic

#Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev mingw-w64 gcc wget gawk

RUN echo "Reload build from here"

#Copy our script and install ocaml + init
COPY ./scripts/install_ocaml_windows.sh ./
RUN printf "\n" | bash -x install_ocaml_windows.sh "--disable-sandboxing -y"

#Copy our script and install build dependencies
COPY ./scripts/install_build_deps_windows.sh ./
RUN bash -x install_build_deps_windows.sh

#Copy our script and install dev dependencies
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

RUN opam install -y js_of_ocaml-compiler.3.4.0 js_of_ocaml-ppx.3.4.0 js_of_ocaml.3.4.0

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
