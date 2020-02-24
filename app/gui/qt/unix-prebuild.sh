#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#dont remove ruby-aubio-prerelease  as needed in linux build
#it is removed in the windows-prebuild
echo "Translating tutorial..."
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/utils/ruby_help.tmpl" "${SCRIPT_DIR}/utils/ruby_help.h"
#assumes linux uses system ruby
#so dont use prefix ../../server/native/ruby/bin/ruby, as unnecessary to set this up
ruby "${SCRIPT_DIR}/../../server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/utils/ruby_help.h"

echo "Updating GUI translation files..."
lrelease "${SCRIPT_DIR}"/lang/*.ts
