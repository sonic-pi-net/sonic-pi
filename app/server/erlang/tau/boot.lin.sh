#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd ${SCRIPT_DIR}

echo "Booting Sonic Pi on Linux..."

TAU_ENABLED=$0 TAU_INTERNAL=$1 TAU_MIDI_ENABLED=$2 TAU_LINK_ENABLED=$3 TAU_IN_PORT=$4 TAU_API_PORT=$5 TAU_SPIDER_PORT=$6 _build\dev\rel\tau\bin\tau start
