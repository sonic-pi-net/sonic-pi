#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$(dirname "$0")"
cd "${SCRIPT_DIR}"

echo "Booting Sonic Pi on Linux..."

TAU_CUES_ON=$1 TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED=$2 TAU_MIDI_ON=$3 TAU_LINK_ON=$4 TAU_OSC_IN_UDP_PORT=$5 TAU_API_PORT=$6 TAU_SPIDER_PORT=$7 TAU_DAEMON_PORT=$8 TAU_LOG_PATH=$9 TAU_MIDI_ENABLED=true TAU_LINK_ENABLED=true _build/"${MIX_ENV:-prod}"/rel/tau/bin/tau start > log/tau_stdout.log 2>&1
