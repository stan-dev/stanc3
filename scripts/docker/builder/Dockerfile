# Pull the ubuntu:jammy base image
FROM ubuntu:jammy

USER root

ARG PUID
ARG PGID

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl ca-certificates wget \
    rsync git build-essential m4 unzip pkg-config libpcre3-dev docker \
    python3 python3-pip nodejs sudo

RUN curl -sSL https://get.docker.com/ | sh

RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

USER jenkins
WORKDIR /home/jenkins
