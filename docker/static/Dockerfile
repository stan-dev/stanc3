FROM ocaml/opam2:alpine
RUN sudo apk update && sudo apk add -q m4 ca-certificates pcre-dev
RUN opam init -y
RUN opam install -y menhir
RUN opam install -y core_kernel ppx_deriving