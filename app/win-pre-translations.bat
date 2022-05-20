set WORKING_DIR=%CD%

cd %~dp0

@echo Translating tutorial...
server\native\ruby\bin\ruby server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y gui\qt\utils\ruby_help.tmpl gui\qt\utils\ruby_help.h
server\native\ruby\bin\ruby server/ruby/bin/qt-doc.rb -o gui\qt\utils/ruby_help.h

cd %WORKING_DIR%
