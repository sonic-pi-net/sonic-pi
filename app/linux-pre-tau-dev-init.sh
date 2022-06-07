#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

MIX_ENV=prod mix local.hex --force
MIX_ENV=prod mix local.rebar --force
MIX_ENV=prod mix deps.get
MIX_ENV=dev mix tailwind.install
MIX_ENV=dev mix esbuild.install


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
