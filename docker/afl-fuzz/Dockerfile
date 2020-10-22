#Pull the ubuntu:bionic image
FROM ubuntu:bionic

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates rsync git build-essential \
  m4 unzip pkg-config libpcre3-dev mingw-w64 \
  gcc wget gawk afl

# Environment variables
ENV TEMP_PATH=tmp

# Install ocaml
RUN curl https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh --output $TEMP_PATH/install.sh
RUN chmod +x $TEMP_PATH/install.sh
RUN echo | $TEMP_PATH/install.sh
RUN rm $TEMP_PATH/install.sh

# Initialize it without sandboxing and create 4.07.0+afl switch
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.07.0+afl
RUN opam switch 4.07.0+afl

# Install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN opam update; bash -x install_build_deps.sh

# Set our work directory
#WORKDIR /stanc3

# We're not copying the source-code into the container as we mount it at runtime
#COPY . .
#COPY ./test/integration/good/code-gen/*.stan ./

# Build stanc.exe
#RUN eval $(opam config env); dune build src/stanc/stanc.exe

# Execute afl-fuzz on runtime
#CMD afl-fuzz -i ./ -o ./afl-out -- _build/default/src/stanc/stanc.exe --debug-parse @@

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]