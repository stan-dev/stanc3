FROM ocaml/opam2:alpine
RUN sudo apk update && sudo apk add -q m4 ca-certificates pcre-dev
RUN printf "\n" | opam -y update
RUN printf "\n" | opam -y install menhir
RUN printf "\n" | opam -y install core_kernel