#!/bin/sh -e

echo "##############################################################################"
echo "## Downloading mingw"
echo "##############################################################################"

case "$ARCH" in
	amd64)
		MINGW_URI="https://bintray.com/libgit2/build-dependencies/download_file?file_path=mingw-w64-x86_64-8.1.0-release-win32-seh-rt_v6-rev0.zip";;
	x86)
		MINGW_URI="https://bintray.com/libgit2/build-dependencies/download_file?file_path=mingw-w64-i686-8.1.0-release-win32-sjlj-rt_v6-rev0.zip";;
esac

curl -s -L "$MINGW_URI" -o "$TEMP"/mingw-"$ARCH".zip
unzip -q "$TEMP"/mingw-"$ARCH".zip -d "$TEMP"
