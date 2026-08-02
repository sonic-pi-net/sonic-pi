#!/bin/bash
# Shared helpers for the mac-release-*.sh stage scripts.
#
# Source from each stage like this (every stage script lives in app/ next to
# this file):
#
#   SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#   source "${SCRIPT_DIR}/mac-release-common.sh"

set -euo pipefail

# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------
# This file lives at <repo>/app/mac-release-common.sh, so APP_DIR is its own
# directory.
APP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_DIR="$( cd "${APP_DIR}/.." && pwd )"

RELEASE_BUILD_DIR="${APP_DIR}/build/macOS_Release"
RELEASE_APP_NAME="Sonic Pi"
RELEASE_APP="${RELEASE_BUILD_DIR}/${RELEASE_APP_NAME}.app"
# Distribution stem — combined with arch + version by release_dmg_basename().
RELEASE_DMG_NAME="Sonic-Pi-for-Mac"
RELEASE_BUNDLE_ID="net.sonic-pi.app"
RELEASE_ENTITLEMENTS="${APP_DIR}/mac-release-entitlements.plist"

# Where the GUI build drops the freshly-built .app
GUI_BUILT_APP="${APP_DIR}/build/gui/${RELEASE_APP_NAME}.app"

# Notarytool keychain profile name (override via env if needed)
RELEASE_NOTARY_PROFILE="${SONIC_PI_NOTARY_PROFILE:-sonic-pi-notary}"

# ----------------------------------------------------------------------------
# Logging
# ----------------------------------------------------------------------------
_color_reset=$'\033[0m'
_color_blue=$'\033[1;34m'
_color_green=$'\033[1;32m'
_color_yellow=$'\033[1;33m'
_color_red=$'\033[1;31m'
_color_dim=$'\033[2m'

log_step()    { printf '%s==>%s %s\n'      "${_color_blue}"   "${_color_reset}" "$*" >&2; }
log_info()    { printf '    %s\n'           "$*" >&2; }
log_ok()      { printf '%s ok %s %s\n'     "${_color_green}"  "${_color_reset}" "$*" >&2; }
log_warn()    { printf '%swarn%s %s\n'     "${_color_yellow}" "${_color_reset}" "$*" >&2; }
log_err()     { printf '%serr %s %s\n'     "${_color_red}"    "${_color_reset}" "$*" >&2; }
log_dim()     { printf '%s%s%s\n'          "${_color_dim}"    "$*" "${_color_reset}" >&2; }

die() {
    log_err "$*"
    exit 1
}

# ----------------------------------------------------------------------------
# Version
# ----------------------------------------------------------------------------
release_version() {
    local raw
    raw="$(cat "${REPO_DIR}/VERSION" 2>/dev/null || echo "")"
    [ -n "$raw" ] || die "VERSION file missing or empty"
    printf '%s' "$raw"
}

# Strip "-dev", "-rc1" etc. from a version string for CFBundleShortVersionString
release_version_short() {
    release_version | sed -E 's/-.*$//'
}

# Insert a hyphen between an alpha pre-release tag and its trailing number
# so VERSION="5.0.0-beta2" → "5.0.0-beta-2" (also "5.0.0-RC1" → "5.0.0-RC-1").
# Used in distribution filenames.
release_version_dist() {
    release_version | sed -E 's/-([A-Za-z]+)([0-9]+)$/-\1-\2/'
}

# DMG basename without the .dmg extension:
#   Sonic-Pi-for-Mac-x64-v5.0.0-beta-2
#   Sonic-Pi-for-Mac-arm64-v5.0.0-beta-2
release_dmg_basename() {
    local arch
    case "$(uname -m)" in
        x86_64) arch="x64" ;;
        *)      arch="$(uname -m)" ;;
    esac
    printf '%s-%s-v%s' \
        "${RELEASE_DMG_NAME}" \
        "$arch" \
        "$(release_version_dist)"
}

# ----------------------------------------------------------------------------
# Codesign identity discovery
# ----------------------------------------------------------------------------
# Resolve a Developer ID Application identity from the user's login keychain.
# Override with env SONIC_PI_RELEASE_IDENTITY (full quoted identity string).
# Returns identity string on stdout; returns "-" (ad-hoc) if SONIC_PI_RELEASE_ADHOC=1.
release_identity() {
    if [ "${SONIC_PI_RELEASE_ADHOC:-0}" = "1" ]; then
        printf '%s' "-"
        return 0
    fi

    if [ -n "${SONIC_PI_RELEASE_IDENTITY:-}" ]; then
        printf '%s' "${SONIC_PI_RELEASE_IDENTITY}"
        return 0
    fi

    local line
    line="$(security find-identity -v -p codesigning 2>/dev/null \
        | grep -E '"Developer ID Application:' \
        | head -n 1 || true)"

    if [ -z "$line" ]; then
        die "No 'Developer ID Application' identity found in keychain.
    Either install one, or set SONIC_PI_RELEASE_IDENTITY=\"Developer ID Application: ...\",
    or set SONIC_PI_RELEASE_ADHOC=1 to ad-hoc sign for local testing."
    fi

    # Extract the quoted identity
    printf '%s' "$line" | sed -E 's/^[^"]*"([^"]+)".*$/\1/'
}

release_team_id() {
    local identity
    identity="$(release_identity)"
    if [ "$identity" = "-" ]; then
        printf '%s' ""
        return 0
    fi
    # Extract "(TEAMID)" from "Developer ID Application: Name (TEAMID)"
    printf '%s' "$identity" | sed -E 's/.*\(([A-Z0-9]+)\)$/\1/'
}

# ----------------------------------------------------------------------------
# Mach-O helpers
# ----------------------------------------------------------------------------
# True if path is a Mach-O file (executable, dylib, or bundle).
is_macho() {
    local f="$1"
    [ -f "$f" ] || return 1
    # file -b returns "Mach-O ..." for any Mach-O. dSYM payloads are also Mach-O
    # but they live inside .dSYM dirs which we strip earlier.
    file -b "$f" 2>/dev/null | grep -q '^Mach-O' || return 1
}

# List all Mach-O files in the bundle, NUL-separated for safe iteration.
list_macho_files() {
    local root="$1"
    find "$root" -type f \( \
        -name '*.dylib' -o \
        -name '*.so'    -o \
        -name '*.bundle' -o \
        -perm -u+x \
    \) -print0 \
    | while IFS= read -r -d '' f; do
        if is_macho "$f"; then
            printf '%s\0' "$f"
        fi
    done
}

# ----------------------------------------------------------------------------
# Misc
# ----------------------------------------------------------------------------
require_macos() {
    [ "$(uname -s)" = "Darwin" ] || die "These scripts only run on macOS"
}

require_cmd() {
    local cmd="$1"
    command -v "$cmd" >/dev/null 2>&1 || die "Required command not found: $cmd"
}

# Run a command and exit with its status, but log the command first when SONIC_PI_RELEASE_VERBOSE=1.
run() {
    if [ "${SONIC_PI_RELEASE_VERBOSE:-0}" = "1" ]; then
        log_dim "+ $*"
    fi
    "$@"
}

# ----------------------------------------------------------------------------
# Native payload allowlist
# ----------------------------------------------------------------------------
# Verify the staged app/server/native root against an allowlist manifest.
# Bash counterpart to install/windows/stage-native.ps1: the native root is
# gitignored and wholesale-copied, so without a check a stale binary from a
# prior build (e.g. the pre-rename `supersonic`) silently ships and gets
# codesigned. Every top-level entry must be covered by a file glob, a
# `dir:` entry, or an `ignore:` glob; each required file glob must match at
# least one file and each `dir:` must exist. Anything else fails the release.
#
#   verify_native_manifest <staged-native-root> <manifest-path>
verify_native_manifest() {
    local native_root="$1"
    local manifest="$2"
    [ -d "$native_root" ] || die "native root not found: $native_root"
    [ -f "$manifest" ]    || die "native manifest not found: $manifest"

    local file_globs=() dir_entries=() ignore_globs=()
    local line stripped
    while IFS= read -r line || [ -n "$line" ]; do
        stripped="${line%%#*}"                                   # drop comments
        # trim leading/trailing whitespace
        stripped="${stripped#"${stripped%%[![:space:]]*}"}"
        stripped="${stripped%"${stripped##*[![:space:]]}"}"
        [ -n "$stripped" ] || continue
        case "$stripped" in
            dir:*)
                stripped="${stripped#dir:}"
                stripped="${stripped#"${stripped%%[![:space:]]*}"}"
                dir_entries+=("$stripped") ;;
            ignore:*)
                stripped="${stripped#ignore:}"
                stripped="${stripped#"${stripped%%[![:space:]]*}"}"
                ignore_globs+=("$stripped") ;;
            *)
                file_globs+=("$stripped") ;;
        esac
    done < "$manifest"

    local errors=0 entry name matched g d
    # Empty-array expansions abort under `set -u` on bash 3.2 (macOS), so guard
    # every "${arr[@]}" with the ${arr[@]+...} form below.

    # 1) Every entry in the native root must be accounted for. Include dotfiles.
    shopt -s nullglob dotglob
    for entry in "$native_root"/*; do
        name="$(basename "$entry")"

        # ignore: globs apply to both files and directories
        matched=0
        for g in ${ignore_globs[@]+"${ignore_globs[@]}"}; do
            [[ "$name" == $g ]] && { matched=1; break; }
        done
        [ "$matched" = 1 ] && continue

        if [ -d "$entry" ]; then
            matched=0
            for d in ${dir_entries[@]+"${dir_entries[@]}"}; do
                [ "$d" = "$name" ] && { matched=1; break; }
            done
            [ "$matched" = 1 ] || { log_err "  native: unexpected directory not in manifest: ${name}/"; errors=$((errors+1)); }
        else
            matched=0
            for g in ${file_globs[@]+"${file_globs[@]}"}; do
                [[ "$name" == $g ]] && { matched=1; break; }
            done
            [ "$matched" = 1 ] || { log_err "  native: unexpected file not in manifest: ${name}"; errors=$((errors+1)); }
        fi
    done
    shopt -u nullglob dotglob

    # 2) Every required file glob must match >= 1 staged root file.
    for g in ${file_globs[@]+"${file_globs[@]}"}; do
        matched=0
        shopt -s nullglob
        for entry in "$native_root"/*; do
            [ -f "$entry" ] || continue
            [[ "$(basename "$entry")" == $g ]] && { matched=1; break; }
        done
        shopt -u nullglob
        [ "$matched" = 1 ] || { log_err "  native: manifest requires '${g}' but no matching file was staged"; errors=$((errors+1)); }
    done

    # 3) Every dir: entry must exist.
    for d in ${dir_entries[@]+"${dir_entries[@]}"}; do
        [ -d "${native_root}/${d}" ] || { log_err "  native: manifest requires dir '${d}/' but it is missing"; errors=$((errors+1)); }
    done

    [ "$errors" = 0 ] || die "native payload failed manifest verification (${errors} problem(s)) — see above"
}
