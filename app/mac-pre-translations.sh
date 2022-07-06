#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

# Check to see if we have a bundled Ruby and if so, use that
# Otherwise use system ruby
BUNDLED_RUBY="${SCRIPT_DIR}/server/native/ruby/bin/ruby"
if [ -f "$BUNDLED_RUBY" ]; then
    RUBY=$BUNDLED_RUBY
else
    echo "Using system Ruby"
    RUBY=ruby
fi

cd "${SCRIPT_DIR}"

echo "Translating tutorial..."
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/i18n-tool.rb -t

echo "Generating docs for the Qt GUI..."
cp "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.tmpl "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h
"$RUBY" "${SCRIPT_DIR}"/server/ruby/bin/qt-doc.rb -o "${SCRIPT_DIR}"/gui/qt/utils/ruby_help.h


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
