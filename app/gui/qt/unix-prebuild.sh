#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

rm -r "${SCRIPT_DIR}/../../server/ruby/vendor/ruby-aubio-prerelease" | true

echo "Translating tutorial..."
../../server/native/ruby/bin/ruby "${SCRIPT_DIR}/../../server/ruby/bin/i18n-tool.rb" -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}/utils/ruby_help.tmpl" "${SCRIPT_DIR}/utils/ruby_help.h"
../../server/native/ruby/bin/ruby "${SCRIPT_DIR}/../../server/ruby/bin/qt-doc.rb" -o "${SCRIPT_DIR}/utils/ruby_help.h"

echo "Updating GUI translation files..."
lrelease "${SCRIPT_DIR}"/lang/*.ts
