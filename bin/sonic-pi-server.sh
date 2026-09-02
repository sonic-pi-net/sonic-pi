#!/bin/bash
# Sonic Pi Server Mode — no auth, POST /run to code over HTTP
# Usage: ./bin/sonic-pi-server.sh [--host 127.0.0.1] [--port 8000]
set -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$( cd "${SCRIPT_DIR}/.." && pwd )"
RUBY_PATH="${ROOT_DIR}/app/server/native/ruby/bin/ruby"
if [ ! -x "${RUBY_PATH}" ]; then
  RUBY_PATH="${ROOT_DIR}/ruby/bin/ruby"
fi
if [ ! -x "${RUBY_PATH}" ]; then
  RUBY_PATH="ruby"
fi
exec "${RUBY_PATH}" "${ROOT_DIR}/app/server/ruby/bin/sonic-pi-server.rb" "$@"
