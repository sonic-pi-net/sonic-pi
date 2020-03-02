#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "Warning: Unix build scripts are still a work in progress!"

# Build external dependencies
if [ "$1" = "--build-aubio" ]; then
  "${SCRIPT_DIR}/external/unix_build_externals.sh" --build-aubio
else
  "${SCRIPT_DIR}/external/unix_build_externals.sh"
fi

# Install dependencies to server
echo "Copying external dependencies to the server..."
mkdir -p "${SCRIPT_DIR}/../../server/native/osmid"
cp "${SCRIPT_DIR}/external/build/osmid-prefix/src/osmid-build/o2m" "${SCRIPT_DIR}/../../server/native/osmid/"
cp "${SCRIPT_DIR}/external/build/osmid-prefix/src/osmid-build/m2o" "${SCRIPT_DIR}/../../server/native/osmid/"

if [ "$1" = "--build-aubio" ]; then
  mkdir -p "${SCRIPT_DIR}/../../server/native/lib"
  cp "${SCRIPT_DIR}/external/build/aubio-prefix/src/aubio-build/libaubio-5.so" "${SCRIPT_DIR}/../../server/native/lib/"
fi

#dont remove ruby-aubio-prerelease, as needed in linux build
#it is removed in the windows-prebuild

echo "Compiling native ruby extensions..."
ruby "${SCRIPT_DIR}/../../server/ruby/bin/compile-extensions.rb"

echo "Compiling erlang files..."
cd "${SCRIPT_DIR}/../../server/erlang"
erlc osc.erl pi_server.erl
cd "${SCRIPT_DIR}"

echo "Translating tutorial..."
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/utils/ruby_help.tmpl" "${SCRIPT_DIR}/utils/ruby_help.h"
ruby "${SCRIPT_DIR}/../../server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/utils/ruby_help.h"

echo "Updating GUI translation files..."
lrelease "${SCRIPT_DIR}"/lang/*.ts
