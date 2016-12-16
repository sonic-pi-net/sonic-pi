
FROM ubuntu:16.04

MAINTAINER Joseph Burnett (josephburnett79@gmail.com)

ADD . sonic-pi

RUN apt-get update
RUN apt-get install -y sudo wget curl python libgit2-dev
RUN cd /sonic-pi/app/gui/qt && ./build-ubuntu-app

