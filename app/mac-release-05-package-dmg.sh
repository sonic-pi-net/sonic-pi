#!/bin/bash
# Stage 5 — package the signed .app into a UDZO disk image.
#
# Layout in the dmg:
#   /Volumes/Sonic Pi/
#       Sonic Pi.app
#       Applications -> /Applications        (drag-to-install affordance)
#
# UDZO is the standard read-only zlib-compressed format used by virtually
# every distributed macOS app dmg. The dmg itself gets codesigned (also a
# notarisation requirement).

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ] || die "Bundle not staged yet — run 00-stage first"

VERSION_FULL="$(release_version)"
DMG_NAME="${RELEASE_DMG_NAME}-${VERSION_FULL}.dmg"
DMG_PATH="${RELEASE_BUILD_DIR}/${DMG_NAME}"
STAGING="${RELEASE_BUILD_DIR}/dmg-staging"
VOLUME_NAME="${RELEASE_APP_NAME}"

IDENTITY="$(release_identity)"

log_step "build dmg staging dir"
rm -rf "${STAGING}"
mkdir -p "${STAGING}"
log_info "  copy ${RELEASE_APP_NAME}.app"
# Use cp -R so we preserve the codesign'd structure exactly. ditto would also
# work but cp -R is fine here since we control both endpoints.
cp -R "${RELEASE_APP}" "${STAGING}/"
log_info "  add /Applications symlink"
ln -s /Applications "${STAGING}/Applications"

log_step "create dmg: ${DMG_NAME}"
rm -f "${DMG_PATH}"
hdiutil create \
    -volname "${VOLUME_NAME}" \
    -srcfolder "${STAGING}" \
    -ov \
    -format UDZO \
    -fs HFS+ \
    -imagekey zlib-level=9 \
    "${DMG_PATH}" >/dev/null

# Clean up staging now that the dmg is built — the bundle still exists at
# ${RELEASE_APP} for re-runs of later stages.
rm -rf "${STAGING}"

log_step "sign dmg"
if [ "${IDENTITY}" = "-" ]; then
    log_warn "ad-hoc signing dmg (SONIC_PI_RELEASE_ADHOC=1) — won't be notarisable"
    codesign --force --sign - "${DMG_PATH}"
else
    codesign --force --timestamp --sign "${IDENTITY}" "${DMG_PATH}"
fi

# Quick verify
codesign --verify --verbose=2 "${DMG_PATH}" 2>&1 | sed 's/^/    /' >&2

dmg_size="$(du -h "${DMG_PATH}" | awk '{print $1}')"
log_ok "stage 05 done — ${DMG_PATH} (${dmg_size})"
