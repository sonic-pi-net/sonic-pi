#!/bin/bash
# Stage 2 — make the bundle portable.
#
# Walks every Mach-O file in the bundle and rewrites any reference to a
# Homebrew-/local-/absolute-path dylib so that the dylib is copied into
# Contents/Frameworks/ and looked up via @rpath. Each binary that needed
# bundling also gets an LC_RPATH entry pointing back to Contents/Frameworks/
# (relative to its own location, via @loader_path).
#
# Replaces the hand-rolled libcrypto.3.dylib fix in the old mac-release.sh
# with a generic walker that catches every offender — Ruby's openssl/psych
# bundles, Erlang's crypto NIF, and anything else that compiled against
# Homebrew on the build machine.
#
# What we leave alone:
#   - /usr/lib/* and /System/* (Apple-shipped, present on every Mac)
#   - @rpath/* @loader_path/* @executable_path/* (already portable)
#   - Anything inside Contents/Frameworks/*.framework/ (Qt — handled by
#     macdeployqt, already self-consistent)

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ] || die "Bundle not staged yet — run 00-stage first"

FRAMEWORKS_DIR="${RELEASE_APP}/Contents/Frameworks"
mkdir -p "${FRAMEWORKS_DIR}"

# basenames of dylibs we've already copied into Frameworks/, to avoid
# re-processing on transitive recursion. Plain array for Bash 3.2 (system bash).
SEEN=()

seen() {
    local x="$1" s
    [ ${#SEEN[@]} -eq 0 ] && return 1
    for s in "${SEEN[@]}"; do
        [ "$s" = "$x" ] && return 0
    done
    return 1
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
is_external_dep() {
    case "$1" in
        /usr/lib/*|/System/*|@rpath/*|@loader_path/*|@executable_path/*)
            return 1 ;;
        /*)
            return 0 ;;
        *)
            return 1 ;;
    esac
}

# Path from a binary's directory up to Contents/Frameworks/ (using ..).
# Computed via python3 so we don't roll our own path arithmetic.
relative_to_frameworks() {
    local binary="$1"
    local binary_dir
    binary_dir="$(cd "$(dirname "$binary")" && pwd)"
    /usr/bin/python3 -c '
import os, sys
print(os.path.relpath(sys.argv[1], sys.argv[2]))
' "${FRAMEWORKS_DIR}" "${binary_dir}"
}

# True if the binary already has the given rpath.
has_rpath() {
    local binary="$1"
    local target="$2"
    otool -l "$binary" 2>/dev/null \
        | awk '/LC_RPATH/{found=1; next} found && /path /{print $2; found=0}' \
        | grep -Fxq "$target"
}

# Strip any existing signature so install_name_tool can rewrite the file.
unsign() {
    codesign --remove-signature "$1" 2>/dev/null || true
}

# Bundle every external dep of $1 (a Mach-O file). Recurses into anything
# we copy into Frameworks/.
# Extract the dependency paths from `otool -L`. Skips:
#   - binary-path headers (no leading whitespace; one per architecture in fat
#     binaries — these confused the older `tail -n +2 | awk '{print $1}'`).
#   - the ` (compatibility version ... current version ...)` suffix, so that
#     paths containing spaces (e.g. ".../Sonic Pi.app/...") survive intact.
otool_deps() {
    otool -L "$1" 2>/dev/null | sed -nE '
        /^[[:space:]]+/!d
        s/^[[:space:]]+//
        s/ \(compatibility version [^)]*\)//
        p
    '
}

# Look up an alternative source for a given dylib basename. Used to redirect
# libssl.* and libcrypto.* away from Homebrew's bottle (which has the wrong
# minos) and toward our private OpenSSL build (which honours
# MACOSX_DEPLOYMENT_TARGET). Returns the override path if it exists, empty
# otherwise.
private_source_for() {
    local base="$1"
    case "$base" in
        libssl.*.dylib|libcrypto.*.dylib)
            local p="${APP_DIR}/external/openssl-build/install/lib/${base}"
            [ -f "$p" ] && printf '%s' "$p"
            ;;
    esac
}

bundle_deps_of() {
    local binary="$1"
    chmod u+w "$binary" 2>/dev/null || true

    # If this binary is a dylib whose own LC_ID_DYLIB still points to an
    # external path (e.g. a copy that was vendored into the bundle pre-staged,
    # like Ruby's libyaml under server/native/ruby/...), rewrite the id to
    # @rpath/<base>. install_name_tool -change handles LC_LOAD_DYLIB only, so
    # the install name has to be fixed separately or the verify step trips.
    local own_id
    own_id="$(otool -D "$binary" 2>/dev/null | sed -n '2p')"
    if [ -n "$own_id" ] && is_external_dep "$own_id"; then
        unsign "$binary"
        install_name_tool -id "@rpath/$(basename "$own_id")" "$binary" 2>/dev/null \
            || log_warn "  could not rewrite id of ${binary##${RELEASE_APP}/}"
    fi

    local deps_text
    deps_text="$(otool_deps "$binary")"

    local needs_rpath=0
    local dep base target src
    while IFS= read -r dep; do
        [ -n "$dep" ] || continue
        if is_external_dep "$dep"; then
            base="$(basename "$dep")"
            target="${FRAMEWORKS_DIR}/${base}"

            if ! seen "$base"; then
                SEEN+=("$base")
                # Prefer our private build over the binary's recorded path
                # for libraries we know need a controlled deployment target.
                src="$(private_source_for "$base")"
                if [ -z "$src" ]; then
                    src="$dep"
                fi
                if [ ! -f "$src" ]; then
                    log_warn "  ${binary##${RELEASE_APP}/}: dep not found on disk: ${src}"
                    continue
                fi
                log_info "  + ${base}  (from ${src})"
                cp "$src" "$target"
                chmod u+w "$target"
                unsign "$target"
                # Set the dylib's own LC_ID_DYLIB so other binaries' references
                # resolve identically.
                install_name_tool -id "@rpath/${base}" "$target" 2>/dev/null
                # Recurse: this dylib may itself depend on other Homebrew libs.
                bundle_deps_of "$target"
            fi

            unsign "$binary"
            install_name_tool -change "$dep" "@rpath/${base}" "$binary" 2>/dev/null
            needs_rpath=1
        fi
    done <<<"$deps_text"

    if [ "$needs_rpath" = "1" ]; then
        local rel="$(relative_to_frameworks "$binary")"
        local rpath="@loader_path/${rel}"
        if ! has_rpath "$binary" "$rpath"; then
            install_name_tool -add_rpath "$rpath" "$binary" 2>/dev/null \
                || log_warn "  could not add rpath to ${binary##${RELEASE_APP}/}"
        fi
    fi
}

# ---------------------------------------------------------------------------
# Walk the bundle
# ---------------------------------------------------------------------------
log_step "scan bundle for external dylib references"

# We enumerate Mach-O files explicitly rather than `find -perm +x` because
# .bundle and .so files often aren't marked executable.
processed=0
while IFS= read -r -d '' f; do
    # Skip anything already inside Frameworks/<x>.framework/ — macdeployqt has
    # already wired Qt's internal references, and we'll add Qt's leaf dylibs to
    # SEEN as a side effect when other binaries reference them.
    case "$f" in
        "${FRAMEWORKS_DIR}/"*.framework/*) continue ;;
    esac
    bundle_deps_of "$f"
    processed=$((processed + 1))
done < <(list_macho_files "${RELEASE_APP}")

log_info "  scanned ${processed} Mach-O files"
log_info "  bundled ${#SEEN[@]} dylibs into Contents/Frameworks/"

# ---------------------------------------------------------------------------
# Sanity check — no /opt or /usr/local references should remain (except
# inside Qt frameworks, which we explicitly skip).
# ---------------------------------------------------------------------------
log_step "verify no external paths remain"
violations=0
while IFS= read -r -d '' f; do
    case "$f" in
        "${FRAMEWORKS_DIR}/"*.framework/*) continue ;;
    esac
    while IFS= read -r dep; do
        case "$dep" in
            /opt/*|/usr/local/*)
                log_err "  ${f##${RELEASE_APP}/}  ->  ${dep}"
                violations=$((violations + 1)) ;;
        esac
    done < <(otool_deps "$f")
done < <(list_macho_files "${RELEASE_APP}")

if [ "$violations" -gt 0 ]; then
    die "Bundle still references ${violations} external paths. Fix and re-run."
fi

log_ok "stage 02 done — ${#SEEN[@]} dylibs bundled, no external paths remain"
