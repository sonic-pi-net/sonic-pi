set WORKING_DIR=%CD%

cd %~dp0

@echo Translating tutorial...
server\native\ruby\bin\ruby server/ruby/bin/i18n-tool.rb -t
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

@echo Generating docs for the Qt GUI...
copy /Y gui\utils\ruby_help.tmpl gui\utils\ruby_help.h
server\native\ruby\bin\ruby server/ruby/bin/qt-doc.rb
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cd %WORKING_DIR%
exit /b 0
