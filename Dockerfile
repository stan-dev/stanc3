FROM ocaml/opam2:alpine
RUN sudo apk update && apk add -q m4 ca-certificates pcre-dev
RUN printf "\n" | opam install menhir
RUN printf "\n" | opam install core_kernel
