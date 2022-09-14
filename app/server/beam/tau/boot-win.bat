cd %~dp0

rem Ensure release has been created with:
rem   mix release

@echo Booting Tau on Windows...

IF "%TAU_ENV%" == "prod" (
  _build\prod\rel\tau\bin\tau start > NUL 2>&1
)

IF "%TAU_ENV%" == "dev" (
  mix assets.deploy.dev
  mix run --no-halt > log\tau_stdout.log 2>&1
)

IF "%TAU_ENV%" == "test" (
  set TAU_MIDI_ENABLED=false
  set TAU_LINK_ENABLED=false
  mix run --no-halt > log\tau_stdout.log 2>&1
)
