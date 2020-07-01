#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "Warning: Unix build scripts are still a work in progress!"

# Check prerequisites for Erlang server
echo -n "Checking Erlang version (>= 23)..."
ERLANG_VERSION=$("${SCRIPT_DIR}/../../server/erlang/print_erlang_version")
if [[ "${ERLANG_VERSION}" < "23" ]]; then
  echo " Fail! Erlang ${ERLANG_VERSION} detected."
  echo "WARNING: Found Erlang version < 23 (${ERLANG_VERSION})! Erlang 23 or above is required for midi functionality, please update your erlang version!"
  echo "Skipping Erlang OSC/MIDI server..."
else
  echo " Pass. Erlang ${ERLANG_VERSION} detected."
  echo "Compiling erlang files..."
  cd "${SCRIPT_DIR}/../../server/erlang/sonic_pi_server"
  erl -make
  cp src/sonic_pi_server.app.src ebin/sonic_pi_server.app
  cd "${SCRIPT_DIR}"
fi

# Build external dependencies
if [ "$1" = "--build-aubio" ]; then
  "${SCRIPT_DIR}/external/linux_build_externals.sh" --build-aubio
else
  "${SCRIPT_DIR}/external/linux_build_externals.sh"
fi

# Install dependencies to server
echo "Copying external dependencies to the server..."
mkdir -p "${SCRIPT_DIR}/../../server/erlang/sonic_pi_server/priv/"
cp "${SCRIPT_DIR}/external/build/sp_midi-prefix/src/sp_midi-build"/*.so "${SCRIPT_DIR}/../../server/erlang/sonic_pi_server/priv/"

if [ "$1" = "--build-aubio" ]; then
  mkdir -p "${SCRIPT_DIR}/../../server/native/lib"
  cp "${SCRIPT_DIR}/external/build/aubio-prefix/src/aubio-build/libaubio-5.so" "${SCRIPT_DIR}/../../server/native/lib/"
fi

#dont remove ruby-aubio-prerelease, as needed in linux build
#it is removed in the windows-prebuild

echo "Compiling native ruby extensions..."
ruby "${SCRIPT_DIR}/../../server/ruby/bin/compile-extensions.rb"

echo "Translating tutorial..."
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/utils/ruby_help.tmpl" "${SCRIPT_DIR}/utils/ruby_help.h"
ruby "${SCRIPT_DIR}/../../server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/utils/ruby_help.h"

echo "Updating GUI translation files..."
lrelease "${SCRIPT_DIR}"/lang/*.ts
