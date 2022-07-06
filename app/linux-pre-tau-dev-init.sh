#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"/server/beam/tau

MIX_ENV=dev mix local.hex --force
MIX_ENV=dev mix local.rebar --force
MIX_ENV=dev mix deps.get
MIX_ENV=dev mix tailwind.install
MIX_ENV=dev mix esbuild.install
MIX_ENV=dev mix assets.deploy.prod

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
