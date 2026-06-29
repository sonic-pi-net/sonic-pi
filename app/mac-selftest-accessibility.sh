#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Run the in-app macOS accessibility self-test: drives the real NSAccessibility
# bridge to confirm the code-completion popup is hidden from the accessibility
# tree and that navigation announcements are delivered. Prints findings and
# exits 0 on PASS. Local-only (needs a window-server session); may require a
# one-time Privacy > Accessibility grant for the app.

APP="${SCRIPT_DIR}/build/gui/Sonic Pi.app/Contents/MacOS/Sonic Pi"

if [ ! -x "${APP}" ]; then
    echo "Build not found at: ${APP}"
    echo "Build the GUI first (e.g. app/mac-build-gui.sh)."
    exit 2
fi

exec "${APP}" --selftest-accessibility
