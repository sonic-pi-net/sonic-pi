cd %~dp0

rmdir /S /Q ..\..\server\ruby\vendor\ruby-aubio-prerelease

echo "Translating tutorial..."
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/i18n-tool.rb -t

echo "Generating docs for the Qt GUI..."
copy /Y utils\ruby_help.tmpl utils\ruby_help.h
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/qt-doc.rb -o utils/ruby_help.h

echo "Updating GUI translation files..."
forfiles /p lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION%/bin/lrelease @file"
