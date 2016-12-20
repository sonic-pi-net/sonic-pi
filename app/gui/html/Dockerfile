
FROM ubuntu:16.04

MAINTAINER Joseph Burnett (josephburnett79@gmail.com)

ADD . sonic-pi

RUN apt-get update
RUN apt-get install -y \
  curl \
  default-jre \
  jackd2 \
  libgit2-dev \
  python \
  sudo \
  wget
RUN cd /sonic-pi/app/gui/qt && ./build-ubuntu-app

