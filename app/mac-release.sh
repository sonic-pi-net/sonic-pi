#!/bin/bash
# mac-release.sh — orchestrator for the macOS release pipeline.
#
# Stages run in order, each is also runnable standalone:
#   00-stage          stage the .app from build/gui/ into build/macOS_Release/
#   01-prune          prune vendor docs/tests, dSYM, headers; flatten symlinks
#   02-bundle-dylibs  copy /opt/* deps into Frameworks/, rewrite to @rpath
#   03-info-plist     set version, copyright, LSMinimumSystemVersion, etc.
#   04-codesign       deep-sign every binary with hardened runtime + entitlements
#   05-package-dmg    build a UDZO dmg with /Applications symlink, sign it
#   06-notarize       submit dmg to Apple, wait, staple
#   07-verify         codesign --verify, spctl --assess, stapler validate
#   08-compat-audit   verify every Mach-O honours LSMinimumSystemVersion
#
# Usage:
#   ./mac-release.sh                        # run all stages
#   ./mac-release.sh all                    # same
#   ./mac-release.sh 02-bundle-dylibs       # just one stage
#   ./mac-release.sh 02 03 04               # range of stages by number prefix
#   ./mac-release.sh --skip-notarize        # all but notarize/staple (offline)
#   ./mac-release.sh --help
#
# Env overrides:
#   SONIC_PI_RELEASE_IDENTITY    full quoted "Developer ID Application: ..."
#   SONIC_PI_RELEASE_ADHOC=1     ad-hoc sign instead (no real signing/notary)
#   SONIC_PI_NOTARY_PROFILE      keychain profile name (default sonic-pi-notary)
#   SONIC_PI_RELEASE_VERBOSE=1   log every external command

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"
trap 'cd "${WORKING_DIR}"' EXIT

source "${SCRIPT_DIR}/mac-release-common.sh"

require_macos

ALL_STAGES=(
    "00-stage"
    "01-prune"
    "02-bundle-dylibs"
    "03-info-plist"
    "04-codesign"
    "05-package-dmg"
    "06-notarize"
    "07-verify"
    "08-compat-audit"
)

usage() {
    sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//' >&2
    exit "${1:-0}"
}

# Parse args
SKIP_NOTARIZE=0
REQUESTED=()
for arg in "$@"; do
    case "$arg" in
        -h|--help)
            usage 0 ;;
        --skip-notarize)
            SKIP_NOTARIZE=1 ;;
        all|"")
            REQUESTED=("${ALL_STAGES[@]}") ;;
        *)
            REQUESTED+=("$arg") ;;
    esac
done

# If no positional stages given, default to all
if [ ${#REQUESTED[@]} -eq 0 ]; then
    REQUESTED=("${ALL_STAGES[@]}")
fi

# Resolve each requested token to a full stage name (allow "02" -> "02-bundle-dylibs")
resolve_stage() {
    local token="$1"
    for s in "${ALL_STAGES[@]}"; do
        if [ "$s" = "$token" ]; then
            printf '%s' "$s"; return 0
        fi
    done
    for s in "${ALL_STAGES[@]}"; do
        if [[ "$s" == "$token"* ]]; then
            printf '%s' "$s"; return 0
        fi
    done
    return 1
}

RESOLVED=()
for token in "${REQUESTED[@]}"; do
    s="$(resolve_stage "$token")" || die "Unknown stage: $token (known: ${ALL_STAGES[*]})"
    RESOLVED+=("$s")
done

if [ "$SKIP_NOTARIZE" = "1" ]; then
    FILTERED=()
    for s in "${RESOLVED[@]}"; do
        case "$s" in 06-notarize) ;; *) FILTERED+=("$s") ;; esac
    done
    RESOLVED=("${FILTERED[@]}")
fi

log_step "Sonic Pi macOS release — stages: ${RESOLVED[*]}"
log_info "Version:    $(release_version)"
log_info "Identity:   $(release_identity)"
log_info "Team ID:    $(release_team_id)"
log_info "Bundle ID:  ${RELEASE_BUNDLE_ID}"
log_info "Output:     ${RELEASE_BUILD_DIR}"

for s in "${RESOLVED[@]}"; do
    stage_script="${SCRIPT_DIR}/mac-release-${s}.sh"
    [ -x "$stage_script" ] || die "Missing or not executable: $stage_script"
    log_step "stage ${s}"
    "$stage_script"
done

log_ok "release pipeline finished"
