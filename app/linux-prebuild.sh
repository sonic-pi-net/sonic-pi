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
        -n|--no-imgui)
            shift
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

if [ ! "$system_libs" == true ]; then
  "${SCRIPT_DIR}"/linux-pre-vcpkg.sh "${args[@]}"
	"${SCRIPT_DIR}"/linux-pre-vcpkg.sh "${args[@]}"
fi

echo "Compiling native ruby extensions..."
ruby "${SCRIPT_DIR}"/server/ruby/bin/compile-extensions.rb

"${SCRIPT_DIR}"/linux-pre-translations.sh "${args[@]}"
