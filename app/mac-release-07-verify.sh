#!/bin/bash
# Stage 7 — final verification of the signed, notarised, stapled output.
#
# Runs the same checks Gatekeeper runs when a user double-clicks the dmg or
# launches the .app for the first time. If anything here fails, end-users
# would also see a failure dialog.
#
# When stage 06 hasn't been run yet, spctl/stapler checks that depend on a
# notarisation ticket are downgraded from failures to warnings — the local
# dev workflow should still get a clean exit so we know codesign passed.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

VERSION_FULL="$(release_version)"
DMG_PATH="${RELEASE_BUILD_DIR}/$(release_dmg_basename).dmg"

[ -d "${RELEASE_APP}" ] || die "Bundle missing: ${RELEASE_APP}"

failures=0
check() {
    local label="$1"; shift
    log_step "${label}"
    if "$@" 2>&1 | sed 's/^/    /' >&2; then
        log_ok "${label}"
    else
        log_err "FAILED: ${label}"
        failures=$((failures + 1))
    fi
}

# Detect notarisation up front so we know whether to gate Gatekeeper-level
# checks or treat them as warnings.
app_stapled=0
dmg_stapled=0
xcrun stapler validate "${RELEASE_APP}" >/dev/null 2>&1 && app_stapled=1
[ -f "${DMG_PATH}" ] && xcrun stapler validate "${DMG_PATH}" >/dev/null 2>&1 && dmg_stapled=1

# ---------------------------------------------------------------------------
# .app checks
# ---------------------------------------------------------------------------
check "codesign --verify --deep --strict on .app" \
    codesign --verify --deep --strict --verbose=2 "${RELEASE_APP}"

if [ "$app_stapled" = "1" ]; then
    check "stapler validate on .app" \
        xcrun stapler validate "${RELEASE_APP}"
    check "spctl --assess on .app (Gatekeeper)" \
        spctl --assess --type exec --verbose=4 "${RELEASE_APP}"
else
    log_warn "no notarisation ticket on .app — skipping stapler/spctl"
    log_warn "  (run stage 06 to notarise + staple before shipping)"
fi

# ---------------------------------------------------------------------------
# dmg checks
# ---------------------------------------------------------------------------
if [ -f "${DMG_PATH}" ]; then
    check "codesign --verify on dmg" \
        codesign --verify --verbose=2 "${DMG_PATH}"

    if [ "$dmg_stapled" = "1" ]; then
        check "stapler validate on dmg" \
            xcrun stapler validate "${DMG_PATH}"
        check "spctl --assess on dmg (Gatekeeper)" \
            spctl --assess --type open --context context:primary-signature -v "${DMG_PATH}"
    else
        log_warn "no notarisation ticket on dmg — skipping stapler/spctl"
    fi
else
    log_warn "no dmg at ${DMG_PATH} — stage 05 not run"
fi

# ---------------------------------------------------------------------------
# Summary — capture codesign output once so grep -q's early-exit doesn't
# SIGPIPE the producer (which under pipefail would look like a failure).
# ---------------------------------------------------------------------------
log_step "summary"

cs_out="$(codesign -dvv "${RELEASE_APP}" 2>&1 || true)"
authority="$(printf '%s\n' "$cs_out" | sed -nE 's/^Authority=(.*)/\1/p' | head -1)"
team_id="$(printf  '%s\n' "$cs_out" | sed -nE 's/^TeamIdentifier=(.*)/\1/p')"
hardened=no;    printf '%s\n' "$cs_out" | grep -qE 'flags=.*runtime'    && hardened=yes
timestamped=no; printf '%s\n' "$cs_out" | grep -q '^Timestamp='         && timestamped=yes
notarised=no;   [ "$app_stapled" = "1" ] && notarised=yes

{
    echo "  bundle:        ${RELEASE_APP}"
    echo "  version:       ${VERSION_FULL}"
    [ -f "${DMG_PATH}" ] && echo "  dmg:           ${DMG_PATH} ($(du -h "${DMG_PATH}" | awk '{print $1}'))"
    echo
    echo "  identity:      ${authority}"
    echo "  team id:       ${team_id}"
    echo "  hardened:      ${hardened}"
    echo "  timestamped:   ${timestamped}"
    echo "  notarised:     ${notarised}"
    [ "$dmg_stapled" = "1" ] && echo "  dmg stapled:   yes"
} >&2

if [ "${failures}" -gt 0 ]; then
    die "${failures} verification check(s) failed"
fi

if [ "$app_stapled" = "0" ]; then
    log_warn "release pipeline incomplete: notarisation pending (run stage 06)"
fi

log_ok "stage 07 done"
