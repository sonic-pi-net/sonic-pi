#!/bin/bash

echo "Fetching dependencies via apt..."

sudo apt-get update
sudo apt-get install -y build-essential libssl-dev git ruby-dev elixir erlang-dev qttools5-dev qttools5-dev-tools libqt5svg5-dev supercollider-server sc3-plugins-server alsa-utils jackd2 libjack-jackd2-0 pulseaudio-module-jack librtmidi-dev cmake ninja-build

# Dependencies for building Erlang/Elixir via asdf...
sudo apt-get -y install build-essential autoconf m4 libncurses5-dev libwxgtk3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop
