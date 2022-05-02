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

cd "${SCRIPT_DIR}"

if [ "$no_imgui" == true ]
then
  "${SCRIPT_DIR}"/linux-pre-vcpkg.sh -n
else
  "${SCRIPT_DIR}"/linux-pre-vcpkg.sh
fi

# Build external dependencies and copy to build tree
echo "Building external binary dependencies..."
"${SCRIPT_DIR}"/external/linux_build_externals.sh

echo "Compiling native ruby extensions..."
ruby "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

"${SCRIPT_DIR}"/linux-pre-translations.sh
"${SCRIPT_DIR}"/linux-pre-copy-binaries.sh
"${SCRIPT_DIR}"/linux-pre-tau-prod-release.sh

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
