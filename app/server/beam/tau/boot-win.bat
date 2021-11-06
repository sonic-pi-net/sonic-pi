cd %~dp0

rem Ensure release has been created with:
rem   mix release

@echo Booting Sonic Pi on Windows...

set TAU_ENABLED=%1%
set TAU_INTERNAL=%2%
set TAU_MIDI_ON=%3%
set TAU_LINK_ON=%4%
set TAU_IN_PORT=%5%
set TAU_API_PORT=%6%
set TAU_SPIDER_PORT=%7%
set TAU_DAEMON_PORT=%8%
set TAU_LOG_PATH=%9%

IF NOT DEFINED MIX_ENV SET "MIX_ENV=prod"

_build\%MIX_ENV%\rel\tau\bin\tau start > "%9%" 2>&1
