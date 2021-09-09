cd %~dp0

rem Ensure release has been created with:
rem   mix release

@echo Booting Sonic Pi on Windows...

set TAU_ENABLED=%1%
set TAU_INTERNAL=%2%
set TAU_MIDI_ENABLED=%3%
set TAU_LINK_ENABLED=%4%
set TAU_IN_PORT=%5%
set TAU_API_PORT=%6%
set TAU_SPIDER_PORT=%7%

IF NOT DEFINED MIX_ENV SET "MIX_ENV=dev"

_build\%MIX_ENV%\rel\tau\bin\tau start
