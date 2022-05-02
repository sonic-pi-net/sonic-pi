#!/bin/bash
set -e # Quit script on error

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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

# Check to see if we have a bundled Ruby and if so, use that
# Otherwise use system ruby
BUNDLED_RUBY="${SCRIPT_DIR}"/server/native/ruby/bin/ruby
if [ -f "$BUNDLED_RUBY" ]; then
    echo "Found bundled Ruby: ${BUNDLED_RUBY}"
    RUBY=$BUNDLED_RUBY
else
    echo "Using system Ruby"
    RUBY=ruby
fi

cd "${SCRIPT_DIR}"

if [ "$no_imgui" == true ]
then
  "${SCRIPT_DIR}"/mac-pre-vcpkg.sh -n
else
  "${SCRIPT_DIR}"/mac-pre-vcpkg.sh
fi

"${SCRIPT_DIR}"/external/mac_build_externals.sh

echo "Compiling native ruby extensions..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

"${SCRIPT_DIR}"/mac-pre-translations.sh
"${SCRIPT_DIR}"/mac-pre-copy-binaries.sh
"${SCRIPT_DIR}"/mac-pre-tau-prod-release.sh

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
