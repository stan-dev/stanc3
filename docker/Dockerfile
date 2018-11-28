FROM ubuntu:bionic
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl ca-certificates rsync git build-essential m4 unzip

COPY ./setup_dev_env.sh ./setup_dev_env.sh
RUN printf "\n" | bash -x setup_dev_env.sh "--disable-sandboxing -y"