#!/bin/bash

## Note: set the SONIC_PI_HOME env variable to specify the location of the log files
##       otherwise it will default to Sonic Pi's standard location in the home directory

set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    cd "${WORKING_DIR}"
}

RUBY_PATH="${SCRIPT_DIR}/../app/server/native/ruby/bin/ruby"
if [ ! -x "${RUBY_PATH}" ]; then
  RUBY_PATH="ruby"
fi


cd "${SCRIPT_DIR}"
"${RUBY_PATH}" "../app/server/ruby/bin/repl.rb" "$@"

# Restore working directory as it was prior to this script running...
trap cleanup_function EXIT
