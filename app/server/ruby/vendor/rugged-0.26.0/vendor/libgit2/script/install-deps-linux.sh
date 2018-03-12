#!/bin/sh

set -x

if [ -z "$PRECISE" ]; then
    echo "deb http://libgit2deps.edwardthomson.com trusty libgit2deps" | sudo tee -a /etc/apt/sources.list
    sudo apt-key adv --keyserver pgp.mit.edu --recv 99131CD5
    sudo apt-get update -qq
    sudo apt-get install -y curl libcurl3 libcurl3-gnutls libcurl4-gnutls-dev
fi

sudo apt-get install -y cmake libssh2-1-dev openssh-client openssh-server valgrind
