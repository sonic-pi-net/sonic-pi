#!/bin/bash

echo "Fetching dependencies via apt..."

sudo apt-get update
sudo apt-get install -y build-essential git libssl-dev ruby-dev elixir erlang-dev erlang-xmerl qt6-tools-dev qt6-tools-dev-tools libqt6svg6-dev libqt6opengl6-dev supercollider-server sc3-plugins-server alsa-utils  libasound2-dev cmake ninja-build pipewire-jack libspa-0.2-jack qt6-wayland libwayland-dev libxkbcommon-dev libegl1-mesa-dev libx11-dev libxft-dev libxext-dev qpwgraph compton