#!/bin/bash -e
shopt -s dotglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

cd "${SCRIPT_DIR}"

# Make dist directory
rm -rf build/linux_dist
mkdir -p build/linux_dist

# Copy distributable files
cp -r ../{bin,etc} build/linux_dist/
cp ../VERSION build/linux_dist/

# Wavetables (~70MB) — not currently used by Sonic Pi, skip from packaging.
rm -rf build/linux_dist/etc/wavetables

# Copy example configs
mkdir -p build/linux_dist/app
cp -r config build/linux_dist/app/

# Copy server natives
mkdir -p build/linux_dist/app/server
cp -r server/native build/linux_dist/app/server/

# Copy Spider (Ruby) server
mkdir -p build/linux_dist/app/server
cp -r server/ruby build/linux_dist/app/server/

# Copy only necessary files for the Qt GUI
mkdir -p build/linux_dist/app/gui/
cp -r gui/{lang,theme} build/linux_dist/app/gui/

# Copy Qt GUI binary
mkdir -p build/linux_dist/app/build/gui/
cp build/gui/sonic-pi build/linux_dist/app/build/gui/sonic-pi

# If ImGui was built
if [ -x build/gui/imgui/sonic-pi-imgui ]; then
  # Copy ImGui files
  mkdir -p build/linux_dist/app/gui/imgui/res
  cp -r gui/imgui/res/Cousine-Regular.ttf build/linux_dist/app/gui/imgui/res/

  # Copy ImGui binary
  mkdir -p build/linux_dist/app/build/gui/imgui
  cp build/gui/imgui/sonic-pi-imgui build/linux_dist/app/build/gui/imgui/sonic-pi-imgui
fi

# Remove non-essential files from vendored Ruby gems
for file in build/linux_dist/app/server/ruby/vendor/*/*; do
  if [ "$(basename "$file")" != "lib" ]; then
    rm -rf "$file"
  fi
done

echo
echo "app/build/linux_dist is now ready for packaging"
echo
