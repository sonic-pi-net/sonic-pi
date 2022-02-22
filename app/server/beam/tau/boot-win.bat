cd %~dp0

rem Ensure release has been created with:
rem   mix release

@echo Booting Tau on Windows...

set TAU_CUES_ON=%1%
set TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED=%2%
set TAU_MIDI_ON=%3%
set TAU_LINK_ON=%4%
set TAU_OSC_IN_UDP_PORT=%5%
set TAU_API_PORT=%6%
set TAU_SPIDER_PORT=%7%
set TAU_DAEMON_PORT=%8%
set TAU_KEEP_ALIVE_PORT=%9%
shift
set TAU_LOG_PATH=%9%
shift
set TAU_MIDI_ENABLED=%9%
shift
set TAU_LINK_ENABLED=%9%
shift
set TAU_PHX_PORT=%9%
shift
set SECRET_KEY_BASE=%9%
shift
set TAU_DAEMON_TOKEN=%9%
shift
set TAU_ENV=%9%

set MIX_ENV=%TAU_ENV%

IF "%TAU_ENV%" == "dev" (
  mix run --no-halt > log\tau_stdout.log 2>&1
) ELSE (
  _build\prod\rel\tau\bin\tau start > NUL 2>&1
)
