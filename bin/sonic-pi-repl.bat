@echo off
REM Note: set the SONIC_PI_HOME env variable to specify the location of the log files
REM       otherwise it will default to Sonic Pi's standard location in the home directory

set SCRIPT_DIR=%~dp0
set WORKING_DIR=%cd%

set RUBY_PATH=%SCRIPT_DIR%\..\app\server\native\ruby\bin\ruby.exe
if not exist "%RUBY_PATH%" (
  set RUBY_PATH=ruby
)

cd "%SCRIPT_DIR%"
"%RUBY_PATH%" "..\app\server\ruby\bin\repl.rb"

REM Restore working directory as it was prior to this script running...
echo Goodbye...
cd "%WORKING_DIR%"
