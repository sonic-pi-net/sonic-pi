#!/bin/bash
set -eux # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}/build/"
mkdir -p macOS_Release
rm -rf macOS_Release
mkdir -p macOS_Release

cp -R 'gui/qt/Sonic Pi.app' macOS_Release/

cd "macOS_Release/Sonic Pi.app/Contents/Resources"

rm app etc server
mkdir app
cp -R ../../../../../../app/server app/server
cp -R ../../../../../../app/config app/config
cp -R ../../../../../../etc .
ln -s app/server .

mkdir -p app/gui/qt
cp -R ../../../../../../app/gui/qt/theme app/gui/qt/
cp -R ../../../../../../app/gui/qt/lang app/gui/qt/lang

../../../../../../app/gui/qt/prune.rb app/server/ruby/vendor


echo "

app/build/macOS_Release/Sonic Pi.app is now ready for signing, notarising and releasing...

"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
