FROM ocaml/opam2:alpine
RUN sudo apk update && sudo apk add -q m4 ca-certificates pcre-dev
RUN printf "\n" | opam install -y merlin utop ocp-indent patdiff ocamlformat menhir
