#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

args=("$@")
system_libs=false

# extract options and their arguments into variables.
while [ -n "$1" ]; do
    case "$1" in
        -c|--config)
            shift 2
            ;;
        -s|--system-libs|-o|--offline-build)
            system_libs=true
            shift
            ;;
        --) shift ; break ;;
        *) echo "Invalid argument: $1" ; exit 1 ;;
    esac
done

cd "${SCRIPT_DIR}"

# Check build dependencies up front. A missing library otherwise only shows
# up deep into the build (e.g. libudev, required by the clockwork Rust crate)
# with a confusing error. See BUILD-LINUX.md for the full package list.
missing=()
for cmd in git c++ cmake ruby cargo pkg-config; do
    command -v "${cmd}" >/dev/null 2>&1 || missing+=("command: ${cmd}")
done
if command -v ruby >/dev/null 2>&1 && \
   ! ruby -rrbconfig -e 'exit File.exist?(File.join(RbConfig::CONFIG["rubyhdrdir"], "ruby.h"))'; then
    missing+=("Ruby development headers (ruby-dev)")
fi
if command -v pkg-config >/dev/null 2>&1; then
    for lib in libudev alsa; do
        pkg-config --exists "${lib}" || missing+=("library: ${lib} (pkg-config)")
    done
fi
if [ ${#missing[@]} -gt 0 ]; then
    echo "ERROR: missing build dependencies:"
    printf '       - %s\n' "${missing[@]}"
    echo "       See BUILD-LINUX.md for the packages to install."
    exit 1
fi

echo "Fetching submodules (app/external/supersonic)..."
git -C "${SCRIPT_DIR}/.." submodule update --init --recursive

if [ -n "${SP_SKIP_RUBY_EXTS:-}" ]; then
    echo "SP_SKIP_RUBY_EXTS set — skipping native ruby extension compile."
    rm -rf "${SCRIPT_DIR}"/server/ruby/rb-native
else
    echo "Compiling native ruby extensions..."
    ruby "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb
fi

"${SCRIPT_DIR}"/linux-pre-translations.sh "${args[@]}"
