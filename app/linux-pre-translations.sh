#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

echo "Translating tutorial..."
ruby "${SCRIPT_DIR}"/server/ruby/bin/i18n-tool.rb -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.tmpl "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h
ruby "${SCRIPT_DIR}"/server/ruby/bin/qt-doc.rb -o "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
