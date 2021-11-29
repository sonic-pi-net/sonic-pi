#!/bin/bash

echo "Fetching dependencies via apt..."

sudo apt-get update
sudo apt-get install -y build-essential libssl-dev git ruby-dev elixir erlang-dev qttools5-dev qttools5-dev-tools libqt5svg5-dev supercollider-server sc3-plugins-server alsa-utils jackd2 libjack-jackd2-0 pulseaudio-module-jack librtmidi-dev cmake ninja-build

# Dependencies for building Erlang/Elixir via asdf...
sudo apt-get -y install build-essential autoconf m4 libncurses5-dev libwxgtk3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop

echo "Fetching qtwebengine debs from upstream Debian as they're not available in Raspbian's packages yet"

mkdir debian-qtwebengine-tmp
cd debian-qtwebengine-tmp
wget  http://ftp.uk.debian.org/debian/pool/main/q/qtwebengine-opensource-src/libqt5webenginecore5_5.15.2+dfsg-3_armhf.deb
wget  http://ftp.uk.debian.org/debian/pool/main/q/qtwebengine-opensource-src/libqt5webengine5_5.15.2+dfsg-3_armhf.deb
wget  http://ftp.uk.debian.org/debian/pool/main/q/qtwebengine-opensource-src/libqt5webenginewidgets5_5.15.2+dfsg-3_armhf.deb
wget  http://ftp.uk.debian.org/debian/pool/main/q/qtwebengine-opensource-src/qtwebengine5-dev_5.15.2+dfsg-3_armhf.deb
sudo apt install -y ./libqt5webenginecore5_5.15.2+dfsg-3_armhf.deb
sudo apt install -y ./libqt5webengine5_5.15.2+dfsg-3_armhf.deb
sudo apt install -y ./libqt5webenginewidgets5_5.15.2+dfsg-3_armhf.deb
sudo apt install -y ./qtwebengine5-dev_5.15.2+dfsg-3_armhf.deb
cd ..
rm -rf debian-qtwebengine-tmp
