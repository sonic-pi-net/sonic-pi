#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"

while getopts ":n" opt; do
  case $opt in
    n)
      no_imgui=true
      echo "Running prebuild script without support for IMGUI-based GUI"
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

# Build vcpkg
if [ ! -d "vcpkg" ]; then
    echo "Cloning vcpkg"
    git clone --depth 1 --branch 2021.05.12  https://github.com/microsoft/vcpkg.git vcpkg
fi

if [ ! -f "vcpkg/vcpkg" ]; then
    echo "Building vcpkg"
    cd vcpkg
    ./bootstrap-vcpkg.sh -disableMetrics
    cd "${SCRIPT_DIR}"
fi

cd vcpkg
triplet=(x64-osx)

if [ $no_imgui == true ]
then
    ./vcpkg install kissfft crossguid platform-folders reproc catch2 --triplet ${triplet[0]} --recurse
else
    ./vcpkg install kissfft fmt sdl2 gl3w reproc gsl-lite concurrentqueue platform-folders catch2 --triplet ${triplet[0]} --recurse
fi

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

echo "echo Cleaning out native dir...."
rm -rf "${SCRIPT_DIR}/server/native/supercollider"
rm -rf "${SCRIPT_DIR}/server/native/erlang"
rm -rf "${SCRIPT_DIR}/server/native/scsynth"


# Build external dependencies and copy to build tree
echo "Building external binary dependencies..."
"${SCRIPT_DIR}/external/mac_build_externals.sh"


cp "${SCRIPT_DIR}"/external/build/aubio-prefix/src/aubio-build/aubio_onset "${SCRIPT_DIR}"/server/native/

mkdir -p "${SCRIPT_DIR}"/server/beam/tau/priv/
for f in "${SCRIPT_DIR}"/external/build/sp_midi-prefix/src/sp_midi-build/*.dylib; do
    cp "$f" "${SCRIPT_DIR}"/server/beam/tau/priv/$(basename "$f" .dylib).so
done

for f in "${SCRIPT_DIR}"/external/build/sp_link-prefix/src/sp_link-build/*.dylib; do
    cp "$f" "${SCRIPT_DIR}"/server/beam/tau/priv/$(basename "$f" .dylib).so
done


# Copy prebuilt native files to server
echo "Copying prebuilt binaries to the server..."
mkdir -p "${SCRIPT_DIR}"/server/native/

cp -R "${SCRIPT_DIR}"/../prebuilt/macos/x64/* "${SCRIPT_DIR}"/server/native/

cd "${SCRIPT_DIR}"/server/native/
ln -s supercollider/scsynth scsynth
mv supercollider/extra-plugins/* supercollider/plugins/
rm -rf supercollider/extra-plugins

echo "Compiling native ruby extensions..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

echo "Translating tutorial..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/i18n-tool.rb -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.tmpl "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/qt-doc.rb -o "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h

echo "Updating GUI translation files..."
# Use lrelease on PATH if available otherwise assume Qt was installed via homebrew
PATH="$PATH":/usr/local/opt/qt@5/bin lrelease "${SCRIPT_DIR}"/gui/qt/lang/*.ts

echo "Compiling Erlang/Elixir files..."
cd "${SCRIPT_DIR}"/server/beam/tau

MIX_ENV=prod mix local.hex --force
MIX_ENV=prod mix local.rebar --force
MIX_ENV=prod mix deps.get
MIX_ENV=prod mix phx.digest
MIX_ENV=prod mix release --overwrite

cp src/tau.app.src ebin/tau.app

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
