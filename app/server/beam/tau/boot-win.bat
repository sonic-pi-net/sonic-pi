cd %~dp0
@echo off

echo Booting Tau on Windows...

IF "%TAU_BOOT_LOG_PATH_WIN%" == "" (
  set TAU_BOOT_LOG_PATH_WIN=log\tau_stdouterr.log
)

IF /I "%TAU_ENV%" == "prod" (
  echo Tau mode: prod
  set MIX_ENV=prod

  rem Ensure release has been created with:
  rem SET MIX_ENV=prod
  rem mix tau.release

  _build\prod\rel\tau\bin\tau start > NUL 2>&1
  goto Exit
)

IF /I "%TAU_ENV%" == "dev" (

  echo Tau mode: dev
  set MIX_ENV=dev

  rem Ensure dev env has been setup with:
  rem SET MIX_ENV=dev
  rem mix setup.dev

  mix assets.deploy.dev
  mix run --no-halt > log\tau_stdout.log 2>&1
  goto Exit
)

IF /I "%TAU_ENV%" == "test" (
  echo Tau mode: test
  set MIX_ENV=test

  set TAU_MIDI_ENABLED=false
  set TAU_LINK_ENABLED=false
  mix run --no-halt > log\tau_stdout.log 2>&1
  goto Exit
)

echo Unknown TAU_ENV environment variable value. Expected one of prod, dev or test.

:Exit
