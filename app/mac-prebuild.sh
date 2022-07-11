#!/bin/bash
set -e # Quit script on error

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

"${SCRIPT_DIR}"/mac-pre-vcpkg.sh "$@"

echo "Compiling native ruby extensions..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

"${SCRIPT_DIR}"/mac-pre-translations.sh

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
