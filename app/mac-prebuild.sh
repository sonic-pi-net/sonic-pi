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

# The server code needs Ruby 3.0+ (it uses endless method definitions).
# macOS still ships Ruby 2.6 at /usr/bin/ruby, which otherwise fails much
# later in the build with confusing syntax errors from qt-doc.rb. Check now,
# before the long vcpkg/OpenSSL steps.
if ! "$RUBY" -e 'exit(Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("3.0") ? 0 : 1)' 2>/dev/null; then
    echo ""
    echo "ERROR: Ruby 3.0 or later is required to build Sonic Pi."
    echo "Found: $("$RUBY" -v 2>/dev/null || echo "no working '$RUBY'") ($(command -v "$RUBY" || echo "$RUBY"))"
    echo ""
    echo "Install a newer Ruby and put it first on your PATH, e.g. with Homebrew:"
    echo "    brew install ruby"
    echo "    export PATH=\"\$(brew --prefix ruby)/bin:\$PATH\""
    echo ""
    exit 1
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
