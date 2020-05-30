#!/usr/bin/env bash
apt-get update
apt-get install --yes libboost-system-dev libboost-program-options-dev libasound2-dev
apt-get install --yes build-essential pkg-config devscripts debhelper git cmake

# And now fetch osmid
rm -rf osmid
git clone https://github.com/llloret/osmid.git
cd osmid
[ -e build ] || mkdir build
cd build
cmake ..
make

