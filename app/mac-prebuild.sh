#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Check to see if we have a bundled Ruby and if so, use that
# Otherwise use system ruby

BUNDLED_RUBY="${SCRIPT_DIR}/server/native/ruby/bin/ruby"
if [ -f "$BUNDLED_RUBY" ]; then
    echo "Found bundled Ruby: ${BUNDLED_RUBY}"
    RUBY=$BUNDLED_RUBY
else
    echo "Using system Ruby"
    RUBY=ruby
fi


# Build external dependencies
"${SCRIPT_DIR}/external/mac_build_externals.sh"
# mkdir -p "${SCRIPT_DIR}/server/native/lib"
 cp "${SCRIPT_DIR}/external/build/aubio-prefix/src/aubio-build/aubio_onset" "${SCRIPT_DIR}/server/native/"


# Install dependencies to server
echo "Copying external dependencies to the server..."
mkdir -p "${SCRIPT_DIR}/server/erlang/sonic_pi_server/priv/"
for f in ${SCRIPT_DIR}/external/build/sp_midi-prefix/src/sp_midi-build/*.dylib; do
    cp $f ${SCRIPT_DIR}/server/erlang/sonic_pi_server/priv/$(basename $f .dylib).so
done


# Copy prebuilt native files to server
echo "Copying prebuilt binaries to the server..."
mkdir -p ${SCRIPT_DIR}/server/native/
rm -rf ${SCRIPT_DIR}/server/native/supercollider
rm -rf ${SCRIPT_DIR}/server/native/erlang
rm -rf ${SCRIPT_DIR}/server/native/scsynth
cp -R ${SCRIPT_DIR}/../prebuilt/macos/x64/* ${SCRIPT_DIR}/server/native/
cd ${SCRIPT_DIR}/server/native/
ln -s supercollider/scsynth scsynth
mv supercollider/extra-plugins/* supercollider/plugins/
rm -rf supercollider/extra-plugins

echo "Compiling native ruby extensions..."
$RUBY "${SCRIPT_DIR}/server/ruby/bin/compile-extensions.rb"

echo "Translating tutorial..."

$RUBY "${SCRIPT_DIR}/server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/gui/qt/utils/ruby_help.tmpl" "${SCRIPT_DIR}/gui/qt/utils/ruby_help.h"
$RUBY "${SCRIPT_DIR}/server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/gui/qt/utils/ruby_help.h"

echo "Updating GUI translation files..."
# Use lrelease on PATH if available otherwise assume Qt was installed via homebrew
PATH=$PATH:/usr/local/opt/qt/bin lrelease "${SCRIPT_DIR}"/gui/qt/lang/*.ts

echo "Compiling erlang files..."
cd "${SCRIPT_DIR}/server/erlang/sonic_pi_server"
../../native/erlang/erl -make
cp src/sonic_pi_server.app.src ebin/sonic_pi_server.app
cd "${SCRIPT_DIR}"
