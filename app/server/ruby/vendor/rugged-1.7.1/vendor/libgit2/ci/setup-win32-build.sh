#!/bin/sh

set -ex

echo "##############################################################################"
echo "## Downloading libssh2"
echo "##############################################################################"

BUILD_TEMP=${BUILD_TEMP:=$TEMP}
BUILD_TEMP=$(cygpath $BUILD_TEMP)

case "$ARCH" in
	amd64)
		LIBSSH2_URI="https://github.com/libgit2/ci-dependencies/releases/download/2023-02-01/libssh2-20230201-amd64.zip";;
	x86)
		LIBSSH2_URI="https://github.com/libgit2/ci-dependencies/releases/download/2023-02-01-v2/libssh2-20230201-x86.zip";;
esac

if [ -z "$LIBSSH2_URI" ]; then
	echo "No URL"
	exit 1
fi

mkdir -p "$BUILD_TEMP"

curl -s -L "$LIBSSH2_URI" -o "$BUILD_TEMP"/libssh2-"$ARCH".zip
unzip -q "$BUILD_TEMP"/libssh2-"$ARCH".zip -d "$BUILD_TEMP"
