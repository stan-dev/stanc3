#Pull the ubuntu:bionic image
FROM ubuntu:bionic

# This image is for cross compiling for windows on debian

# Set the opam root
ENV OPAMROOT /usr/local/opam

#Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates rsync git build-essential m4 \
  unzip pkg-config libpcre3-dev mingw-w64 gcc wget gawk \
  vim nano sudo time

#Copy our script and install ocaml + init
COPY ./scripts/install_ocaml_windows.sh ./
RUN printf "\n" | bash -x install_ocaml_windows.sh "--root=/usr/local/opam --comp 4.02.3 --disable-sandboxing -y"

#Copy our script and install build dependencies
COPY ./scripts/install_build_deps_windows.sh ./
RUN bash -x install_build_deps_windows.sh

#Copy our script and install dev dependencies
# COPY ./scripts/install_dev_deps.sh ./
# RUN bash -x install_dev_deps.sh

RUN opam install -y js_of_ocaml-compiler.3.4.0 js_of_ocaml-ppx.3.4.0 js_of_ocaml.3.4.0

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]
