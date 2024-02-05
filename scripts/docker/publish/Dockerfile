# Pull the ubuntu:bionic base image
FROM ubuntu:bionic

USER root

ARG PUID
ARG PGID

# Install OS dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl ca-certificates wget \
    rsync git build-essential m4 unzip pkg-config libpcre3-dev \
    python3 python3-pip nodejs sudo

RUN addgroup -gid ${PGID} jenkins
RUN adduser --disabled-password --gecos '' --ingroup jenkins --uid ${PUID} jenkins
RUN usermod -a -G sudo jenkins
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R jenkins:sudo /usr/local

USER jenkins
WORKDIR /home/jenkins