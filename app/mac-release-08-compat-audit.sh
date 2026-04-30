#!/bin/bash
# Stage 08 — verify backward compatibility floor.
#
# Walks every Mach-O in the bundle and checks its LC_BUILD_VERSION (or the
# legacy LC_VERSION_MIN_MACOSX on older binaries) against the
# LSMinimumSystemVersion baked into Contents/Info.plist by stage 03.
#
# Why: macOS only consults LSMinimumSystemVersion at app-launch time. Once
# the app is up, every spawned helper / loaded dylib is independently
# version-checked by dyld. If a single binary's `minos` is higher than
# what we declared, dyld refuses to load it on older macOS and the app
# half-boots then dies. We learned this the hard way with `beam.smp` and
# Homebrew's `libssl`/`libcrypto` (built on the host's macOS, so they
# silently inherit a high `minos`).
#
# Run by the orchestrator after the bundle is fully assembled. No
# observable side effects — just exits non-zero with a list of offenders.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ] || die "Bundle not staged yet — run 00-stage first"

require_cmd vtool

plist="${RELEASE_APP}/Contents/Info.plist"
declared="$(plutil -extract LSMinimumSystemVersion raw "$plist" 2>/dev/null || true)"
[ -n "$declared" ] || die "LSMinimumSystemVersion missing from Info.plist (run stage 03)"

# Normalise "13" → "13.0" so version_lte's purely-numeric comparison works
# uniformly on plist values that drop the trailing minor.
case "$declared" in
    *.*) ;;
    *)   declared="${declared}.0" ;;
esac

log_step "compat audit — declared floor: macOS ${declared}"

# Compare two dotted versions: returns 0 if a <= b, 1 otherwise.
# Pure-bash, handles missing minor/patch components.
version_lte() {
    local a="$1" b="$2"
    local ai=0 bi=0
    IFS=. read -r a1 a2 a3 <<<"$a"
    IFS=. read -r b1 b2 b3 <<<"$b"
    ai=$(( ${a1:-0} * 10000 + ${a2:-0} * 100 + ${a3:-0} ))
    bi=$(( ${b1:-0} * 10000 + ${b2:-0} * 100 + ${b3:-0} ))
    [ "$ai" -le "$bi" ]
}

# Extract the minimum macOS version a single Mach-O claims to support.
# Modern binaries use LC_BUILD_VERSION (`minos`), older ones LC_VERSION_MIN_MACOSX.
# Universal binaries report per-architecture; we take the highest.
binary_minos() {
    local f="$1"
    vtool -show "$f" 2>/dev/null \
        | awk '
            /^[[:space:]]*minos[[:space:]]/  { print $2 }
            /^[[:space:]]*version[[:space:]]/ && want_min { print $2; want_min=0 }
            /LC_VERSION_MIN_MACOSX/          { want_min=1 }
        ' \
        | sort -V | tail -1
}

violations=0
checked=0
highest_seen=""

while IFS= read -r -d '' f; do
    minos="$(binary_minos "$f" || true)"
    [ -n "$minos" ] || continue
    checked=$((checked + 1))

    if [ -z "$highest_seen" ] || ! version_lte "$minos" "$highest_seen"; then
        highest_seen="$minos"
    fi

    if ! version_lte "$minos" "$declared"; then
        log_err "  ${minos}  ${f#${RELEASE_APP}/}"
        violations=$((violations + 1))
    fi
done < <(list_macho_files "${RELEASE_APP}")

log_info "  scanned ${checked} Mach-O files; highest minos: ${highest_seen:-none}"

if [ "$violations" -gt 0 ]; then
    log_err "${violations} binar(y/ies) require macOS newer than the declared floor (${declared})."
    log_err "either rebuild those deps with MACOSX_DEPLOYMENT_TARGET=${declared}"
    log_err "or raise LSMinimumSystemVersion in mac-release-03-info-plist.sh."
    exit 1
fi

log_ok "stage 08 done — every Mach-O honours macOS ${declared}"
