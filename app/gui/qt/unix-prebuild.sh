#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "Warning: Unix build scripts are still a work in progress!"

# Build external dependencies
"${SCRIPT_DIR}/external/unix_build_externals.sh"

# Install dependencies to server
echo "Copying external dependencies to the server..."
mkdir -p "${SCRIPT_DIR}/../../server/native/lib"
cp "${SCRIPT_DIR}/external/build/aubio-prefix/src/Aubio-build/libaubio-5.so" "${SCRIPT_DIR}/../../server/native/lib/"

mkdir -p "${SCRIPT_DIR}/../../server/native/osmid"
cp "${SCRIPT_DIR}/external/build/osmid-prefix/src/osmid-build/o2m" "${SCRIPT_DIR}/../../server/native/osmid/"
cp "${SCRIPT_DIR}/external/build/osmid-prefix/src/osmid-build/m2o" "${SCRIPT_DIR}/../../server/native/osmid/"

#dont remove ruby-aubio-prerelease, as needed in linux build
#it is removed in the windows-prebuild

echo "Translating tutorial..."
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/utils/ruby_help.tmpl" "${SCRIPT_DIR}/utils/ruby_help.h"
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/utils/ruby_help.h"

echo "Updating GUI translation files..."
lrelease "${SCRIPT_DIR}"/lang/*.ts
