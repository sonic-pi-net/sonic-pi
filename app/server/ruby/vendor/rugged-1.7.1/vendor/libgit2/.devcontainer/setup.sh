#!/bin/sh
set -e

sudo apt-get update
sudo apt-get -y --no-install-recommends install cmake

mkdir build
cd build
cmake ..