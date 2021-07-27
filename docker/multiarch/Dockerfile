FROM debian:bullseye-slim

RUN apt-get update
RUN apt-get install opam curl bzip2 git tar curl ca-certificates openssl m4 bash -y

RUN opam init --disable-sandboxing -y
RUN eval $(opam env)
RUN opam switch create 4.07.0 && opam switch 4.07.0
RUN eval $(opam env) && opam repo add internet https://opam.ocaml.org

RUN curl https://raw.githubusercontent.com/stan-dev/stanc3/master/scripts/install_build_deps.sh | bash
