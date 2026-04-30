#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

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

echo "Fetching submodules (app/external/supersonic)..."
git -C "${SCRIPT_DIR}/.." submodule update --init --recursive

"${SCRIPT_DIR}"/mac-pre-vcpkg.sh "$@"

# Build OpenSSL ourselves with the correct deployment target. Homebrew's
# bottles (and even `--build-from-source`) bake the host SDK's macOS
# version into LC_BUILD_VERSION minos, which dyld then refuses to load on
# older macOS. Building OpenSSL ourselves into a private prefix is the
# only way to ship a libssl/libcrypto whose minos honours
# MACOSX_DEPLOYMENT_TARGET. Idempotent — skips on subsequent builds.
"${SCRIPT_DIR}"/mac-build-openssl.sh

echo "Compiling native ruby extensions..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

"${SCRIPT_DIR}"/mac-pre-translations.sh
