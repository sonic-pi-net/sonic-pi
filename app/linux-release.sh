#!/bin/bash -e
shopt -s dotglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "${SCRIPT_DIR}"

# Make dist directory
rm -rf build/linux_dist
mkdir -p build/linux_dist

# Copy distributable files
cp -r ../{bin,etc} build/linux_dist/

# Copy example configs
mkdir -p build/linux_dist/app
cp -r config build/linux_dist/app/

# Copy server natives
mkdir -p build/linux_dist/app/server
cp -r server/native build/linux_dist/app/server/

# Copy Spider (Ruby) server
mkdir -p build/linux_dist/app/server
cp -r server/ruby build/linux_dist/app/server/

# Copy built Tau (BEAM) server
mkdir -p build/linux_dist/app/server/beam/tau/_build/prod
cp -r server/beam/tau/_build/prod/rel build/linux_dist/app/server/beam/tau/_build/prod/
cp server/beam/tau/boot-lin.sh build/linux_dist/app/server/beam/tau/

# Copy only necessary files for the Qt GUI
mkdir -p build/linux_dist/app/gui/qt
cp -r gui/qt/{lang,theme} build/linux_dist/app/gui/qt/

# Copy Qt GUI binary
mkdir -p build/linux_dist/app/build/gui/qt
cp build/gui/qt/sonic-pi build/linux_dist/app/build/gui/qt/sonic-pi

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

# Remove unnecessary Erlang artifacts
rm build/linux_dist/app/server/beam/tau/_build/prod/rel/tau/bin/tau.bat

# Strip Erlang BEAMs
erl -noinput -eval \
  'lists:foreach(fun(F) -> beam_lib:strip(F) end, filelib:wildcard("build/linux_dist/app/server/beam/tau/**/*.beam"))' \
  -s init stop

echo
echo "app/build/linux_dist is now ready for packaging"
echo
