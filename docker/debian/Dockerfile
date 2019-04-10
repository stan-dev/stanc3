<<<<<<< HEAD:docker/dev-ubuntu/Dockerfile
#Use Official Docker Images for OCAML/OPAM
#https://github.com/ocaml/infrastructure/wiki/Containers
FROM ocaml/opam2:debian-stable

#Switch to root user so we can install apt packages
USER root

#Install os dependencies
=======
#Pull the ubuntu:bionic image 
FROM ubuntu:bionic

#Install OS dependencies
>>>>>>> b98f92d6a970967bd51158c1b555bf338d4da3b7:docker/debian/Dockerfile
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates \
  rsync git build-essential m4 unzip pkg-config libpcre3-dev

<<<<<<< HEAD:docker/dev-ubuntu/Dockerfile
#Switch back to the normal user
USER opam

#Copy & Run the script that installs the opam build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Copy & Run the script that installs the opam development dependencies
=======
#Copy our script and install ocaml + init
COPY ./scripts/install_ocaml.sh ./
RUN printf "\n" | bash -x install_ocaml.sh "--disable-sandboxing -y"

#Copy our script and install build dependencies
COPY ./scripts/install_build_deps.sh ./
RUN bash -x install_build_deps.sh

#Copy our script and install dev dependencies
>>>>>>> b98f92d6a970967bd51158c1b555bf338d4da3b7:docker/debian/Dockerfile
COPY ./scripts/install_dev_deps.sh ./
RUN bash -x install_dev_deps.sh

#Specify our entrypoint
ENTRYPOINT [ "opam", "config", "exec", "--" ]