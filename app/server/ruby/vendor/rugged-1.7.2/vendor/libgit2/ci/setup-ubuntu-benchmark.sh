#!/bin/sh

set -ex

sudo apt-get update
sudo apt-get install -y --no-install-recommends \
	cargo \
	cmake \
	gcc \
	git \
	krb5-user \
	libkrb5-dev \
	libssl-dev \
	libz-dev \
	make \
	ninja-build \
	pkgconf

wget https://github.com/sharkdp/hyperfine/releases/download/v1.12.0/hyperfine_1.12.0_amd64.deb
sudo dpkg -i hyperfine_1.12.0_amd64.deb
