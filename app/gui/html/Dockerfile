
FROM ubuntu:16.04

MAINTAINER Joseph Burnett (josephburnett79@gmail.com)

ADD . sonic-pi

RUN cp /sonic-pi/app/gui/html/sources.list /etc/apt/
RUN apt-get update
RUN apt-get install -y \
  curl \
  darkice \
  default-jre \
  icecast2 \
  jackd2 \
  libgit2-dev \
  python \
  sudo \
  wget
RUN cd /sonic-pi/app/gui/qt && ./build-ubuntu-app
RUN LEIN_ROOT=1 cd /sonic-pi/app/gui/html && ./vendor/lein cljsbuild once

ENTRYPOINT /sonic-pi/app/server/bin/launch-gce

