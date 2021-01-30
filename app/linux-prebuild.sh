#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "Warning: Unix build scripts are still a work in progress!"



"${SCRIPT_DIR}/external/linux_build_externals.sh"


# Install dependencies to server
echo "Copying external dependencies to the server..."
mkdir -p "${SCRIPT_DIR}/server/erlang/sonic_pi_server/priv/"
cp ${SCRIPT_DIR}/external/build/sp_midi-prefix/src/sp_midi-build/*.so ${SCRIPT_DIR}/server/erlang/sonic_pi_server/priv/

cp "${SCRIPT_DIR}/external/build/aubio-prefix/src/aubio-build/aubio_onset" "${SCRIPT_DIR}/server/native/"

#dont remove ruby-aubio-prerelease, as needed in linux build
#it is removed in the windows-prebuild

echo "Compiling native ruby extensions..."
ruby "${SCRIPT_DIR}/server/ruby/bin/compile-extensions.rb"

echo "Translating tutorial..."
#assumes linux uses system ruby
#so dont use prefix /server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/gui/qt/utils/ruby_help.tmpl" "${SCRIPT_DIR}/gui/qt/utils/ruby_help.h"
ruby "${SCRIPT_DIR}/server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/gui/qt/utils/ruby_help.h"

echo "Updating GUI translation files..."
PATH=`pkg-config --variable bindir Qt5`:$PATH lrelease "${SCRIPT_DIR}"/gui/qt/lang/*.ts

echo "Compiling erlang files..."
cd "${SCRIPT_DIR}/server/erlang/sonic_pi_server"
erl -make
cp src/sonic_pi_server.app.src ebin/sonic_pi_server.app
cd "${SCRIPT_DIR}"
