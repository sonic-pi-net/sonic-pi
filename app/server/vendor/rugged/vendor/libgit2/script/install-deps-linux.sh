#!/bin/sh

set -x

sudo apt-get -qq update &&
sudo apt-get -qq install cmake libssh2-1-dev openssh-client openssh-server
