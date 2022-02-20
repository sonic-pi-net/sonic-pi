#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$(dirname "$0")"
cd "${SCRIPT_DIR}"

echo "Booting Tau on Linux..."

export TAU_CUES_ON=$1
export TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED=$2
export TAU_MIDI_ON=$3
export TAU_LINK_ON=$4
export TAU_OSC_IN_UDP_PORT=$5
export TAU_API_PORT=$6
export TAU_SPIDER_PORT=$7
export TAU_DAEMON_PORT=$8
export TAU_KEEP_ALIVE_PORT=$9
export TAU_LOG_PATH=${10}
export TAU_MIDI_ENABLED=${11}
export TAU_LINK_ENABLED=${12}
export TAU_PHX_PORT=${13}
export SECRET_KEY_BASE=${14}
export TAU_DAEMON_TOKEN=${15}
export TAU_ENV=${16}
export MIX_ENV=$TAU_ENV

if [ $TAU_ENV = "dev" ]
then
  mix run --no-halt > log/tau_stdout.log 2>&1
else
  _build/prod/rel/tau/bin/tau start > /dev/null 2>&1
fi
