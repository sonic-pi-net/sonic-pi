#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"

echo "Compiling Erlang/Elixir files..."
cd "${SCRIPT_DIR}"/server/beam/tau

MIX_ENV=prod mix local.hex --force
MIX_ENV=prod mix local.rebar --force
MIX_ENV=prod mix deps.get
MIX_ENV=prod mix assets.deploy.prod
MIX_ENV=prod mix tailwind.install
MIX_ENV=prod mix esbuild.install
MIX_ENV=prod mix release --overwrite

cp src/tau.app.src ebin/tau.app
