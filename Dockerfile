FROM ocaml/opam2:alpine
RUN sudo apk update && sudo apk add -q m4 ca-certificates pcre-dev
COPY ./scripts/*.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"
RUN bash -x install_dev_deps.sh 