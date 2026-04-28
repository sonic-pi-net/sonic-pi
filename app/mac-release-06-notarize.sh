#!/bin/bash
# Stage 6 — submit the dmg to Apple's notarisation service, wait, staple.
#
# Uses xcrun notarytool with a keychain-stored credential profile. The
# profile is created once per developer with:
#
#   xcrun notarytool store-credentials sonic-pi-notary \
#       --apple-id   you@example.com \
#       --team-id    YOURTEAMID \
#       --password   <app-specific-password from appleid.apple.com>
#
# After successful notarisation we staple the ticket onto the dmg AND the
# .app inside (so the .app is also offline-verifiable once installed).

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

require_cmd xcrun

VERSION_FULL="$(release_version)"
DMG_PATH="${RELEASE_BUILD_DIR}/${RELEASE_DMG_NAME}-${VERSION_FULL}.dmg"
[ -f "${DMG_PATH}" ] || die "DMG missing: ${DMG_PATH} — run stage 05 first"
[ -d "${RELEASE_APP}" ] || die "Bundle missing: ${RELEASE_APP}"

PROFILE="${RELEASE_NOTARY_PROFILE}"

# ---------------------------------------------------------------------------
# Verify the credential profile exists. notarytool stores profiles in the
# keychain and gives no programmatic "list profiles" command, so we do a
# cheap call (history with a tiny limit) and check the error.
# ---------------------------------------------------------------------------
log_step "check notarytool credential profile: ${PROFILE}"
if ! xcrun notarytool history --keychain-profile "${PROFILE}" --output-format json 2>&1 | head -c 1 >/dev/null; then
    cat <<EOF >&2

  Credential profile "${PROFILE}" is missing or invalid.

  Create it once with:

    xcrun notarytool store-credentials "${PROFILE}" \\
        --apple-id   "<your-apple-id@example.com>" \\
        --team-id    "$(release_team_id)" \\
        --password   "<app-specific-password>"

  Get the app-specific password from https://appleid.apple.com under
  "Sign-In and Security" -> "App-Specific Passwords".

  Override the profile name with SONIC_PI_NOTARY_PROFILE if you use a
  different one already.

EOF
    die "set up the profile and re-run stage 06"
fi
log_info "  profile present"

# ---------------------------------------------------------------------------
# Submit
# ---------------------------------------------------------------------------
log_step "submit ${DMG_PATH##*/} to Apple (this can take several minutes)"
log_dim "  this is a network upload + remote verification — sit tight"

# --wait blocks until the submission succeeds or fails; --output-format json
# lets us extract the submission id and final status.
submission_json="$(xcrun notarytool submit "${DMG_PATH}" \
    --keychain-profile "${PROFILE}" \
    --wait \
    --output-format json 2>&1)" || {
    log_err "notarytool submit failed:"
    printf '%s\n' "${submission_json}" | sed 's/^/    /' >&2
    exit 1
}

sub_id="$(printf '%s' "${submission_json}" | /usr/bin/python3 -c 'import sys,json; print(json.load(sys.stdin).get("id",""))' 2>/dev/null || true)"
status="$(printf '%s' "${submission_json}" | /usr/bin/python3 -c 'import sys,json; print(json.load(sys.stdin).get("status",""))' 2>/dev/null || true)"

log_info "  submission id: ${sub_id}"
log_info "  status:        ${status}"

if [ "${status}" != "Accepted" ]; then
    log_err "notarisation status: ${status}"
    log_err "fetching log:"
    xcrun notarytool log "${sub_id}" --keychain-profile "${PROFILE}" 2>&1 | sed 's/^/    /' >&2
    die "notarisation rejected"
fi

# ---------------------------------------------------------------------------
# Staple
# ---------------------------------------------------------------------------
log_step "staple ticket onto dmg"
xcrun stapler staple "${DMG_PATH}" 2>&1 | sed 's/^/    /' >&2 \
    || die "stapler failed for dmg"

log_step "staple ticket onto .app"
xcrun stapler staple "${RELEASE_APP}" 2>&1 | sed 's/^/    /' >&2 \
    || die "stapler failed for .app"

log_ok "stage 06 done — notarised + stapled"
