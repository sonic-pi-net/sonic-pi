#!/bin/sh

set -ex

brew update
brew install pkgconfig zlib curl openssl libssh2 ninja

ln -s /Applications/Xcode.app/Contents/Developer/usr/lib/libLeaksAtExit.dylib /usr/local/lib
